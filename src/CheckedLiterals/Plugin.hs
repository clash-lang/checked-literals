{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module CheckedLiterals.Plugin (plugin) where

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
  SourceText (NoSourceText, SourceText),
  il_value,
 )

import CheckedLiterals.Class.Integer (
  checkedNegativeIntegerLiteral,
  checkedPositiveIntegerLiteral,
 )
import CheckedLiterals.Class.Rational (
  checkedNegativeRationalLiteral,
  checkedPositiveRationalLiteral,
 )
import CheckedLiterals.Unchecked (uncheckedLiteral)
import Data.Ratio qualified as Ratio
import GHC.Types.SourceText qualified as SourceText
import Language.Haskell.TH qualified as TH

data HelperNames = HelperNames
  { checkedPositiveIntegerLiteralName :: Name
  , checkedNegativeIntegerLiteralName :: Name
  , checkedPositiveRationalLiteralName :: Name
  , checkedNegativeRationalLiteralName :: Name
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
    <$> lookupHelper 'checkedPositiveIntegerLiteral
    <*> lookupHelper 'checkedNegativeIntegerLiteral
    <*> lookupHelper 'checkedPositiveRationalLiteral
    <*> lookupHelper 'checkedNegativeRationalLiteral
    <*> lookupHelper 'uncheckedLiteral

lookupHelperModule :: ModuleName -> TcM Module
lookupHelperModule moduleName = do
  hscEnv <- getTopEnv
  case lookupModuleWithSuggestions (hsc_units hscEnv) moduleName NoPkgQual of
    LookupFound foundModule _ -> pure foundModule
    _ -> panic "CheckedLiterals.Plugin: failed to resolve helper module"

quotedNameModuleName :: TH.Name -> ModuleName
quotedNameModuleName name =
  case TH.nameModule name of
    Just moduleName -> mkModuleName moduleName
    Nothing ->
      panic $
        "CheckedLiterals.Plugin: quoted helper name is missing a module: "
          ++ TH.pprint name

-- | Transform a located expression using top-down traversal.
transformLHsExpr :: LHsExpr GhcRn -> TransformM (LHsExpr GhcRn)
transformLHsExpr lexpr@(L loc expr) = do
  helperNames <- ask
  case expr of
    -- Check if this is an application to our checked literal functions. If so, stop recursing
    -- to avoid double transformation.
    HsApp _ fun _ | isCheckedLiteralApp helperNames (unLoc fun) -> return lexpr
    -- Handle negation of fractional literals: detect (negate 3.14) patterns
    NegApp _ (L _ (HsOverLit _ OverLit{ol_val = HsFractional fracLit})) _ -> do
      let
        rational = negate (SourceText.rationalFromFractionalLit fracLit)
        transformedExpr =
          makeCheckedRationalLiteral
            helperNames
            expr
            (fractionalLiteralDisplayText rational fracLit)
            rational
      return (L loc transformedExpr)

    -- Handle negation of integer literals: detect (negate literal) patterns
    NegApp _ (L _ (HsOverLit _ OverLit{ol_val = HsIntegral intLit})) _ -> do
      let
        value = il_value intLit
        transformedExpr = makeCheckedLiteral helperNames expr (negate value)
      return (L loc transformedExpr)

    -- Transform positive fractional literals
    HsOverLit _ OverLit{ol_val = HsFractional fracLit} -> do
      let rational = SourceText.rationalFromFractionalLit fracLit
      return $
        L loc $
          makeCheckedRationalLiteral
            helperNames
            expr
            (fractionalLiteralDisplayText rational fracLit)
            rational

    -- Transform positive integer literals
    HsOverLit _ OverLit{ol_val = HsIntegral intLit} -> do
      let value = il_value intLit
      return $ L loc $ makeCheckedLiteral helperNames expr value

    -- For all other expressions, recurse into children (top-down)
    _ -> L loc <$> gmapM transformData expr

-- | Transform any located pattern, regardless of context.
transformLPat :: LPat GhcRn -> TransformM (LPat GhcRn)
transformLPat lpat@(L loc pat) = do
  helperNames <- ask
  case pat of
    ViewPat _ viewExpr _
      | isCheckedLiteralApp helperNames (unLoc viewExpr) ->
          pure lpat
    NPat _ overLit negation _
      | Just viewExpr <- makeCheckedPatternViewExpr helperNames (unLoc overLit) negation ->
          pure (L loc (ViewPat mkViewPatExt (noLocA viewExpr) lpat))
    _ -> L loc <$> gmapM transformPatData pat

transformPatData :: (Data a) => a -> TransformM a
transformPatData =
  gmapM transformPatData
    `extM` transformLPat
    `extM` transformLHsExpr

makeCheckedPatternViewExpr ::
  HelperNames ->
  HsOverLit GhcRn ->
  Maybe (SyntaxExpr GhcRn) ->
  Maybe (HsExpr GhcRn)
makeCheckedPatternViewExpr helperNames overLit negation =
  case overLit.ol_val of
    HsIntegral intLit ->
      let value = applyPatternNegation negation (il_value intLit)
       in Just (makeCheckedLiteralFunction helperNames value)
    HsFractional fracLit ->
      let rational = applyPatternNegation negation (SourceText.rationalFromFractionalLit fracLit)
       in Just
            ( makeCheckedRationalLiteralFunction
                helperNames
                (fractionalLiteralDisplayText rational fracLit)
                rational
            )
    HsIsString _ _ -> Nothing

applyPatternNegation :: (Num a) => Maybe b -> a -> a
applyPatternNegation Nothing value = value
applyPatternNegation (Just _) value = negate value

mkViewPatExt :: XViewPat GhcRn
mkViewPatExt = Nothing

#if MIN_VERSION_ghc(9,8,0)
unpackFSCompat :: FastString -> String
unpackFSCompat = unpackFS
#else
unpackFSCompat :: String -> String
unpackFSCompat = id
#endif

fractionalLiteralDisplayText :: Rational -> SourceText.FractionalLit -> String
fractionalLiteralDisplayText rational fracLit =
  case SourceText.fl_text fracLit of
    SourceText sourceText ->
      let sourceTextStr = unpackFSCompat sourceText
       in case sourceTextStr of
            '-' : _ -> sourceTextStr
            _ | rational < 0 -> '-' : sourceTextStr
            _ -> sourceTextStr
    NoSourceText ->
      RatioExtra.showFixedPoint rational

{- FOURMOLU_DISABLE -}
-- | Check if an expression is an application to one of our checked literal functions
isCheckedLiteralApp :: HelperNames -> HsExpr GhcRn -> Bool
isCheckedLiteralApp helperNames expr = case expr of
  -- Direct reference to checked literal function
  HsVar _ name -> isCheckedLiteralName helperNames (getNameFromLocatedOcc name)
  -- Parentheses do not change helper identity.
#if MIN_VERSION_ghc(9,10,0)
  HsPar _ innerExpr -> isCheckedLiteralApp helperNames (unLoc innerExpr)
#else
  HsPar _ _ innerExpr _ -> isCheckedLiteralApp helperNames (unLoc innerExpr)
#endif
  -- Type application to checked literal function, e.g.: checkedPositiveIntegerLiteral @N
#if MIN_VERSION_ghc(9,10,0)
  HsAppType _ funExpr _ -> isCheckedLiteralApp helperNames (unLoc funExpr)
#else
  HsAppType _ funExpr _ _ -> isCheckedLiteralApp helperNames (unLoc funExpr)
#endif
  _ -> False
{- FOURMOLU_ENABLE -}

-- | Check if a name is one of our checked literal functions or uncheckedLiteral
isCheckedLiteralName :: HelperNames -> Name -> Bool
isCheckedLiteralName helperNames name =
  name == helperNames.checkedPositiveIntegerLiteralName
    || name == helperNames.checkedNegativeIntegerLiteralName
    || name == helperNames.checkedPositiveRationalLiteralName
    || name == helperNames.checkedNegativeRationalLiteralName
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

-- | Build the expression, e.g.: checkedPositiveIntegerLiteral @N e
makeCheckedLiteral :: HelperNames -> HsExpr GhcRn -> Integer -> HsExpr GhcRn
makeCheckedLiteral helperNames expr value = fullApp
 where
  withTypeApp = makeCheckedLiteralFunction helperNames value
#if MIN_VERSION_ghc(9,10,0)
  fullApp = HsApp noExtField (noLocA withTypeApp) (noLocA expr)
#else
  fullApp = HsApp noAnn (noLocA withTypeApp) (noLocA expr)
#endif

makeCheckedLiteralFunction :: HelperNames -> Integer -> HsExpr GhcRn
makeCheckedLiteralFunction helperNames value = withTypeApp
 where
  funcName
    | value >= 0 = helperNames.checkedPositiveIntegerLiteralName
    | otherwise = helperNames.checkedNegativeIntegerLiteralName
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
checkedPositiveRationalLiteral @"3.14" @314 @100 (3.14)
-}
makeCheckedRationalLiteral :: HelperNames -> HsExpr GhcRn -> String -> Rational -> HsExpr GhcRn
makeCheckedRationalLiteral helperNames expr stringRepr rational = fullApp
 where
  withAllTypeApps = makeCheckedRationalLiteralFunction helperNames stringRepr rational
#if MIN_VERSION_ghc(9,10,0)
  fullApp = HsApp noExtField (noLocA withAllTypeApps) (noLocA expr)
#else
  fullApp = HsApp noAnn (noLocA withAllTypeApps) (noLocA expr)
#endif

makeCheckedRationalLiteralFunction :: HelperNames -> String -> Rational -> HsExpr GhcRn
makeCheckedRationalLiteralFunction helperNames stringRepr rational = withAllTypeApps
 where
  funcName
    | rational >= 0 = helperNames.checkedPositiveRationalLiteralName
    | otherwise = helperNames.checkedNegativeRationalLiteralName
  funcVar = noLocA (HsVar noExtField (mkLocatedOcc funcName))

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
