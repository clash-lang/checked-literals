{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module SafeLiterals.Plugin (plugin) where

import GHC.Hs
import Prelude

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Generics (Data, extM, gmapM)
import Data.Ratio.Extra qualified as RatioExtra
import GHC.Iface.Env (lookupOrig)
import GHC.Plugins hiding (rational, (<>))
import GHC.Tc.Types (TcGblEnv, TcM)
import GHC.Tc.Utils.Monad (getTopEnv)
import GHC.Types.SourceText (
  SourceText (NoSourceText),
  il_value,
 )

import Data.Ratio qualified as Ratio
import GHC.Types.SourceText qualified as SourceText
import Language.Haskell.TH qualified as TH
import SafeLiterals.Class.Integer (
  safeNegativeIntegerLiteral,
  safePositiveIntegerLiteral,
 )
import SafeLiterals.Class.Rational (
  safeNegativeRationalLiteral,
  safePositiveRationalLiteral,
 )
import SafeLiterals.Unchecked (uncheckedLiteral)

data HelperNames = HelperNames
  { safePositiveIntegerLiteralName :: Name
  , safeNegativeIntegerLiteralName :: Name
  , safePositiveRationalLiteralName :: Name
  , safeNegativeRationalLiteralName :: Name
  , uncheckedLiteralName :: Name
  }

type TransformM = Reader HelperNames

-- | The GHC plugin entry point
plugin :: Plugin
plugin =
  defaultPlugin
    { renamedResultAction = renamedPlugin
    , pluginRecompile = purePlugin
    }

-- | Rewrite numeric literals after renaming, using exact Names for helper detection.
renamedPlugin :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
renamedPlugin _opts tcGblEnv hsGroup = do
  helperNames <- loadHelperNames
  let transformedGroup = runReader (transformHsGroup hsGroup) helperNames
  pure (tcGblEnv, transformedGroup)

-- | Top-down traversal of HsGroup, transforming expressions and patterns.
transformHsGroup :: HsGroup GhcRn -> TransformM (HsGroup GhcRn)
transformHsGroup hsGroup = gmapM transformData hsGroup

transformData :: (Data a) => a -> TransformM a
transformData =
  gmapM transformData
    `extM` transformLHsExpr
    `extM` transformLPat

loadHelperNames :: TcM HelperNames
loadHelperNames = do
  let lookupHelper quotedName = do
        helperModule <- lookupHelperModule (quotedNameModuleName quotedName)
        lookupOrig helperModule (mkVarOcc (TH.nameBase quotedName))
  HelperNames
    <$> lookupHelper 'safePositiveIntegerLiteral
    <*> lookupHelper 'safeNegativeIntegerLiteral
    <*> lookupHelper 'safePositiveRationalLiteral
    <*> lookupHelper 'safeNegativeRationalLiteral
    <*> lookupHelper 'uncheckedLiteral

lookupHelperModule :: ModuleName -> TcM Module
lookupHelperModule moduleName = do
  hscEnv <- getTopEnv
  case lookupModuleWithSuggestions (hsc_units hscEnv) moduleName NoPkgQual of
    LookupFound foundModule _ -> pure foundModule
    _ -> panic "SafeLiterals.Plugin: failed to resolve helper module"

quotedNameModuleName :: TH.Name -> ModuleName
quotedNameModuleName name =
  case TH.nameModule name of
    Just moduleName -> mkModuleName moduleName
    Nothing ->
      panic $
        "SafeLiterals.Plugin: quoted helper name is missing a module: "
          ++ TH.pprint name

-- | Transform a located expression using top-down traversal.
transformLHsExpr :: LHsExpr GhcRn -> TransformM (LHsExpr GhcRn)
transformLHsExpr lexpr@(L loc expr) = do
  helperNames <- ask
  case expr of
    -- Check if this is an application to our safe literal functions. If so, stop recursing
    -- to avoid double transformation.
    HsApp _ fun _ | isSafeLiteralApp helperNames (unLoc fun) -> return lexpr
    -- Handle negation of fractional literals: detect (negate 3.14) patterns
    NegApp _ (L _ (HsOverLit _ OverLit{ol_val = HsFractional fracLit})) _ -> do
      let
        rational = negate (SourceText.rationalFromFractionalLit fracLit)
        transformedExpr = makeSafeRationalLiteral helperNames expr rational
      return (L loc transformedExpr)

    -- Handle negation of integer literals: detect (negate literal) patterns
    NegApp _ (L _ (HsOverLit _ OverLit{ol_val = HsIntegral intLit})) _ -> do
      let
        value = il_value intLit
        transformedExpr = makeSafeLiteral helperNames expr (negate value)
      return (L loc transformedExpr)

    -- Transform positive fractional literals
    HsOverLit _ OverLit{ol_val = HsFractional fracLit} -> do
      let rational = SourceText.rationalFromFractionalLit fracLit
      return $ L loc $ makeSafeRationalLiteral helperNames expr rational

    -- Transform positive integer literals
    HsOverLit _ OverLit{ol_val = HsIntegral intLit} -> do
      let value = il_value intLit
      return $ L loc $ makeSafeLiteral helperNames expr value

    -- For all other expressions, recurse into children (top-down)
    _ -> L loc <$> gmapM transformData expr

-- | Transform any located pattern, regardless of context.
transformLPat :: LPat GhcRn -> TransformM (LPat GhcRn)
transformLPat lpat@(L loc pat) = do
  helperNames <- ask
  case pat of
    ViewPat _ viewExpr _
      | isSafeLiteralApp helperNames (unLoc viewExpr) ->
          pure lpat
    NPat _ overLit negation _
      | Just viewExpr <- makeSafePatternViewExpr helperNames (unLoc overLit) negation ->
          pure (L loc (ViewPat mkViewPatExt (noLocA viewExpr) lpat))
    _ -> L loc <$> gmapM transformPatData pat

transformPatData :: (Data a) => a -> TransformM a
transformPatData =
  gmapM transformPatData
    `extM` transformLPat
    `extM` transformLHsExpr

makeSafePatternViewExpr ::
  HelperNames ->
  HsOverLit GhcRn ->
  Maybe (SyntaxExpr GhcRn) ->
  Maybe (HsExpr GhcRn)
makeSafePatternViewExpr helperNames overLit negation =
  case overLit.ol_val of
    HsIntegral intLit ->
      let value = applyPatternNegation negation (il_value intLit)
       in Just (makeSafeLiteralFunction helperNames value)
    HsFractional fracLit ->
      let rational = applyPatternNegation negation (SourceText.rationalFromFractionalLit fracLit)
       in Just (makeSafeRationalLiteralFunction helperNames rational)
    HsIsString _ _ -> Nothing

applyPatternNegation :: (Num a) => Maybe b -> a -> a
applyPatternNegation Nothing value = value
applyPatternNegation (Just _) value = negate value

mkViewPatExt :: XViewPat GhcRn
mkViewPatExt = Nothing

{- FOURMOLU_DISABLE -}
-- | Check if an expression is an application to one of our safe literal functions
isSafeLiteralApp :: HelperNames -> HsExpr GhcRn -> Bool
isSafeLiteralApp helperNames expr = case expr of
  -- Direct reference to safe literal function
  HsVar _ name -> isSafeLiteralName helperNames (getNameFromLocatedOcc name)
  -- Parentheses do not change helper identity.
#if MIN_VERSION_ghc(9,10,0)
  HsPar _ innerExpr -> isSafeLiteralApp helperNames (unLoc innerExpr)
#else
  HsPar _ _ innerExpr _ -> isSafeLiteralApp helperNames (unLoc innerExpr)
#endif
  -- Type application to safe literal function, e.g.: safePositiveIntegerLiteral @N
#if MIN_VERSION_ghc(9,10,0)
  HsAppType _ funExpr _ -> isSafeLiteralApp helperNames (unLoc funExpr)
#else
  HsAppType _ funExpr _ _ -> isSafeLiteralApp helperNames (unLoc funExpr)
#endif
  _ -> False
{- FOURMOLU_ENABLE -}

-- | Check if a name is one of our safe literal functions or uncheckedLiteral
isSafeLiteralName :: HelperNames -> Name -> Bool
isSafeLiteralName helperNames name =
  name == helperNames.safePositiveIntegerLiteralName
    || name == helperNames.safeNegativeIntegerLiteralName
    || name == helperNames.safePositiveRationalLiteralName
    || name == helperNames.safeNegativeRationalLiteralName
    || name == helperNames.uncheckedLiteralName

#if MIN_VERSION_ghc(9,14,0)
getNameFromLocatedOcc :: LIdOccP GhcRn -> Name
getNameFromLocatedOcc = unLocWithUserRdr
#else
getNameFromLocatedOcc :: LIdP GhcRn -> Name
getNameFromLocatedOcc = unLoc
#endif

#if MIN_VERSION_ghc(9,14,0)
mkLocatedOcc :: Name -> LIdOccP GhcRn
mkLocatedOcc = noLocA . noUserRdr
#else
mkLocatedOcc :: Name -> LIdP GhcRn
mkLocatedOcc = noLocA
#endif

-- | Build the expression, e.g.: safePositiveIntegerLiteral @N e
makeSafeLiteral :: HelperNames -> HsExpr GhcRn -> Integer -> HsExpr GhcRn
makeSafeLiteral helperNames expr value = fullApp
 where
  withTypeApp = makeSafeLiteralFunction helperNames value
#if MIN_VERSION_ghc(9,10,0)
  fullApp = HsApp noExtField (noLocA withTypeApp) (noLocA expr)
#else
  fullApp = HsApp noAnn (noLocA withTypeApp) (noLocA expr)
#endif

makeSafeLiteralFunction :: HelperNames -> Integer -> HsExpr GhcRn
makeSafeLiteralFunction helperNames value = withTypeApp
 where
  funcName
    | value >= 0 = helperNames.safePositiveIntegerLiteralName
    | otherwise = helperNames.safeNegativeIntegerLiteralName
  funcVar = noLocA (HsVar noExtField (mkLocatedOcc funcName))
  tyLit = HsNumTy NoSourceText (abs value)
#if MIN_VERSION_ghc(9,10,0)
  typeArg = HsWC [] (noLocA (HsTyLit noExtField tyLit))
  withTypeApp = HsAppType noExtField funcVar typeArg
#else
  typeArg = HsWC [] (noLocA (HsTyLit noExtField tyLit))
  atToken = L NoTokenLoc (HsTok @"@")
  withTypeApp = HsAppType noExtField funcVar atToken typeArg
#endif

{- | Build the expression for rational literals, e.g.:
safePositiveRationalLiteral @"3.14" @314 @100 (3.14)
-}
makeSafeRationalLiteral :: HelperNames -> HsExpr GhcRn -> Rational -> HsExpr GhcRn
makeSafeRationalLiteral helperNames expr rational = fullApp
 where
  withAllTypeApps = makeSafeRationalLiteralFunction helperNames rational
#if MIN_VERSION_ghc(9,10,0)
  fullApp = HsApp noExtField (noLocA withAllTypeApps) (noLocA expr)
#else
  fullApp = HsApp noAnn (noLocA withAllTypeApps) (noLocA expr)
#endif

makeSafeRationalLiteralFunction :: HelperNames -> Rational -> HsExpr GhcRn
makeSafeRationalLiteralFunction helperNames rational = withAllTypeApps
 where
  funcName
    | rational >= 0 = helperNames.safePositiveRationalLiteralName
    | otherwise = helperNames.safeNegativeRationalLiteralName
  funcVar = noLocA (HsVar noExtField (mkLocatedOcc funcName))

  -- Create the rational and get its string representation. This allows errors messages to
  -- show "3.14" instead of "314 % 100".
  stringRepr = RatioExtra.showFixedPoint rational

  -- Type-level literals
  strTyLit = HsStrTy NoSourceText (mkFastString stringRepr)
  numTyLit = HsNumTy NoSourceText (abs (Ratio.numerator rational))
  denTyLit = HsNumTy NoSourceText (abs (Ratio.denominator rational))
#if MIN_VERSION_ghc(9,10,0)
  strTypeArg = HsWC [] (noLocA (HsTyLit noExtField strTyLit))
  numTypeArg = HsWC [] (noLocA (HsTyLit noExtField numTyLit))
  denTypeArg = HsWC [] (noLocA (HsTyLit noExtField denTyLit))
  withStrTypeApp = HsAppType noExtField funcVar strTypeArg
  withNumTypeApp = HsAppType noExtField (noLocA withStrTypeApp) numTypeArg
  withAllTypeApps = HsAppType noExtField (noLocA withNumTypeApp) denTypeArg
#else
  strTypeArg = HsWC [] (noLocA (HsTyLit noExtField strTyLit))
  numTypeArg = HsWC [] (noLocA (HsTyLit noExtField numTyLit))
  denTypeArg = HsWC [] (noLocA (HsTyLit noExtField denTyLit))
  atToken = L NoTokenLoc (HsTok @"@")
  withStrTypeApp = HsAppType noExtField funcVar atToken strTypeArg
  withNumTypeApp = HsAppType noExtField (noLocA withStrTypeApp) atToken numTypeArg
  withAllTypeApps = HsAppType noExtField (noLocA withNumTypeApp) atToken denTypeArg
#endif
