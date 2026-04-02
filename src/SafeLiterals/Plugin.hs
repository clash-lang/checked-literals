{-# LANGUAGE CPP #-}

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

-- | Top-down traversal of HsGroup, transforming expressions.
transformHsGroup :: HsGroup GhcRn -> TransformM (HsGroup GhcRn)
transformHsGroup hsGroup = gmapM transformData hsGroup

transformData :: (Data a) => a -> TransformM a
transformData = gmapM transformData `extM` transformLHsExpr

loadHelperNames :: TcM HelperNames
loadHelperNames = do
  safeIntegerModule <- lookupHelperModule safeIntegerModuleName
  safeRationalModule <- lookupHelperModule safeRationalModuleName
  safeUncheckedModule <- lookupHelperModule safeUncheckedModuleName
  let lookupHelper modName occ = lookupOrig modName (mkVarOcc occ)
  HelperNames
    <$> lookupHelper safeIntegerModule safePositiveIntegerLiteralOcc
    <*> lookupHelper safeIntegerModule safeNegativeIntegerLiteralOcc
    <*> lookupHelper safeRationalModule safePositiveRationalLiteralOcc
    <*> lookupHelper safeRationalModule safeNegativeRationalLiteralOcc
    <*> lookupHelper safeUncheckedModule uncheckedLiteralOcc

lookupHelperModule :: ModuleName -> TcM Module
lookupHelperModule moduleName = do
  hscEnv <- getTopEnv
  case lookupModuleWithSuggestions (hsc_units hscEnv) moduleName NoPkgQual of
    LookupFound foundModule _ -> pure foundModule
    _ -> panic "SafeLiterals.Plugin: failed to resolve helper module"

safeIntegerModuleName :: ModuleName
safeIntegerModuleName = mkModuleName "SafeLiterals.Class.Integer"

safeRationalModuleName :: ModuleName
safeRationalModuleName = mkModuleName "SafeLiterals.Class.Rational"

safeUncheckedModuleName :: ModuleName
safeUncheckedModuleName = mkModuleName "SafeLiterals.Unchecked"

safePositiveIntegerLiteralOcc :: String
safePositiveIntegerLiteralOcc = "safePositiveIntegerLiteral"

safeNegativeIntegerLiteralOcc :: String
safeNegativeIntegerLiteralOcc = "safeNegativeIntegerLiteral"

safePositiveRationalLiteralOcc :: String
safePositiveRationalLiteralOcc = "safePositiveRationalLiteral"

safeNegativeRationalLiteralOcc :: String
safeNegativeRationalLiteralOcc = "safeNegativeRationalLiteral"

uncheckedLiteralOcc :: String
uncheckedLiteralOcc = "uncheckedLiteral"

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

{- FOURMOLU_DISABLE -}
-- | Check if an expression is an application to one of our safe literal functions
isSafeLiteralApp :: HelperNames -> HsExpr GhcRn -> Bool
isSafeLiteralApp helperNames expr = case expr of
  -- Direct reference to safe literal function
  HsVar _ (L _ name) -> isSafeLiteralName helperNames name
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

-- | Build the expression, e.g.: safePositiveIntegerLiteral @N e
makeSafeLiteral :: HelperNames -> HsExpr GhcRn -> Integer -> HsExpr GhcRn
makeSafeLiteral helperNames expr value = fullApp
 where
  funcName
    | value >= 0 = helperNames.safePositiveIntegerLiteralName
    | otherwise = helperNames.safeNegativeIntegerLiteralName
  funcVar = noLocA (HsVar noExtField (noLocA funcName))
  tyLit = HsNumTy NoSourceText (abs value)
#if MIN_VERSION_ghc(9,10,0)
  typeArg = HsWC [] (noLocA (HsTyLit noExtField tyLit))
  withTypeApp = HsAppType noExtField funcVar typeArg
  fullApp = HsApp noExtField (noLocA withTypeApp) (noLocA expr)
#else
  typeArg = HsWC [] (noLocA (HsTyLit noExtField tyLit))
  atToken = L NoTokenLoc (HsTok @"@")
  withTypeApp = HsAppType noExtField funcVar atToken typeArg
  fullApp = HsApp noAnn (noLocA withTypeApp) (noLocA expr)
#endif

{- | Build the expression for rational literals, e.g.:
safePositiveRationalLiteral @"3.14" @314 @100 (3.14)
-}
makeSafeRationalLiteral :: HelperNames -> HsExpr GhcRn -> Rational -> HsExpr GhcRn
makeSafeRationalLiteral helperNames expr rational = fullApp
 where
  funcName
    | rational >= 0 = helperNames.safePositiveRationalLiteralName
    | otherwise = helperNames.safeNegativeRationalLiteralName
  funcVar = noLocA (HsVar noExtField (noLocA funcName))

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
  fullApp = HsApp noExtField (noLocA withAllTypeApps) (noLocA expr)
#else
  strTypeArg = HsWC [] (noLocA (HsTyLit noExtField strTyLit))
  numTypeArg = HsWC [] (noLocA (HsTyLit noExtField numTyLit))
  denTypeArg = HsWC [] (noLocA (HsTyLit noExtField denTyLit))
  atToken = L NoTokenLoc (HsTok @"@")
  withStrTypeApp = HsAppType noExtField funcVar atToken strTypeArg
  withNumTypeApp = HsAppType noExtField (noLocA withStrTypeApp) atToken numTypeArg
  withAllTypeApps = HsAppType noExtField (noLocA withNumTypeApp) atToken denTypeArg
  fullApp = HsApp noAnn (noLocA withAllTypeApps) (noLocA expr)
#endif
