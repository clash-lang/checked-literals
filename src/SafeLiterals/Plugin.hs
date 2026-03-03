{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module SafeLiterals.Plugin (plugin) where

import GHC.Hs
import Prelude

import Control.Monad.State (State, put, runState)
import Data.Generics (Data, extM, gmapM)
import Data.Ratio.Extra qualified as RatioExtra
import GHC.Plugins hiding (rational, (<>))
import GHC.Types.SourceText (
  SourceText (NoSourceText),
  il_value,
 )

import Data.Ratio qualified as Ratio
import GHC.Types.SourceText qualified as SourceText

-- | Type alias for transformation state: tracks whether any changes occurred
type TransformM = State Bool

{- | Mark that we've transformed at least one literal, so we know to inject the import
if needed.
-}
markChanged :: TransformM ()
markChanged = put True

-- | The GHC plugin entry point
plugin :: Plugin
plugin =
  defaultPlugin
    { parsedResultAction = parsedPlugin
    , pluginRecompile = purePlugin
    }

-- | The parsed result action that transforms numeric literals
parsedPlugin :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
parsedPlugin _opts _summary result@ParsedResult{parsedResultModule} = do
  let hsModule = parsedResultModule
  transformedModule <- transformParsedModule hsModule
  -- liftIO $ putStrLn "--- AST After Plugin ---"
  -- liftIO $ showPprUnsafe (hpm_module transformedModule)
  return result{parsedResultModule = transformedModule}

-- | Transform all numeric literals in a parsed module
transformParsedModule :: HsParsedModule -> Hsc HsParsedModule
transformParsedModule hpm = do
  let
    L loc hsmod = hpm_module hpm

    -- Run top-down transformation with State monad
    (transformedHsmod, anyChanged) = runState (transformHsModule hsmod) False

    -- Only inject import if we actually transformed any literals
    newImports
      | anyChanged = mkInsertsImport : hsmodImports transformedHsmod
      | otherwise = hsmodImports transformedHsmod

    finalHsmod = transformedHsmod{hsmodImports = newImports}

  return hpm{hpm_module = L loc finalHsmod}

-- | Top-down traversal of HsModule, transforming expressions
transformHsModule :: HsModule GhcPs -> TransformM (HsModule GhcPs)
transformHsModule hsmod = gmapM transformData hsmod

transformData :: (Data a) => a -> TransformM a
transformData = gmapM transformData `extM` transformLHsExpr

{- | Create an import declaration for SafeLiterals. Uses a real (but empty) SrcSpan so GHC
properly tracks import usage. See https://gitlab.haskell.org/ghc/ghc/-/issues/21730.
-}
mkInsertsImport :: LImportDecl GhcPs
mkInsertsImport = L ann $ simpleImportDecl moduleName
 where
  srcLoc = mkSrcLoc (mkFastString "") 0 0
  srcSpan = mkSrcSpan srcLoc srcLoc
  ann = noAnnSrcSpan srcSpan
  moduleName = mkModuleName "SafeLiterals"

-- | Transform a located expression using top-down traversal.
transformLHsExpr :: LHsExpr GhcPs -> TransformM (LHsExpr GhcPs)
transformLHsExpr lexpr@(L loc expr) = case expr of
  -- Check if this is an application to our safe literal functions. If so, stop recursing
  -- to avoid double transformation.
  HsApp _ fun _ | isSafeLiteralApp (unLoc fun) -> return lexpr
  -- Handle negation of fractional literals: detect (negate 3.14) patterns
  NegApp _ (L _ (HsOverLit _ OverLit{ol_val = HsFractional fracLit})) _ -> do
    markChanged
    let
      rational = negate (SourceText.rationalFromFractionalLit fracLit)
      transformedExpr = makeSafeRationalLiteral expr rational
    return (L loc transformedExpr)

  -- Handle negation of integer literals: detect (negate literal) patterns
  NegApp _ (L _ (HsOverLit _ OverLit{ol_val = HsIntegral intLit})) _ -> do
    markChanged
    let
      value = il_value intLit
      transformedExpr = makeSafeLiteral expr (negate value)
    return (L loc transformedExpr)

  -- Transform positive fractional literals
  HsOverLit _ OverLit{ol_val = HsFractional fracLit} -> do
    markChanged
    let rational = SourceText.rationalFromFractionalLit fracLit
    return $ L loc $ makeSafeRationalLiteral expr rational

  -- Transform positive integer literals
  HsOverLit _ OverLit{ol_val = HsIntegral intLit} -> do
    markChanged
    let value = il_value intLit
    return $ L loc $ makeSafeLiteral expr value

  -- For all other expressions, recurse into children (top-down)
  _ -> L loc <$> gmapM transformData expr

{- FOURMOLU_DISABLE -}
-- | Check if an expression is an application to one of our safe literal functions
isSafeLiteralApp :: HsExpr GhcPs -> Bool
isSafeLiteralApp expr = case expr of
  -- Direct reference to safe literal function
  HsVar _ (L _ rdrName) -> isSafeLiteralName rdrName
  -- Type application to safe literal function, e.g.: safePositiveIntegerLiteral @N
#if MIN_VERSION_ghc(9,10,0)
  HsAppType _ funExpr _ -> isSafeLiteralApp (unLoc funExpr)
#else
  HsAppType _ funExpr _ _ -> isSafeLiteralApp (unLoc funExpr)
#endif
  _ -> False
{- FOURMOLU_ENABLE -}

-- | Check if a name is one of our safe literal functions or uncheckedLiteral
isSafeLiteralName :: RdrName -> Bool
isSafeLiteralName rdrName =
  case rdrName of
    Unqual (occNameString -> name) ->
      name == "safePositiveIntegerLiteral"
        || name == "safeNegativeIntegerLiteral"
        || name == "safePositiveRationalLiteral"
        || name == "safeNegativeRationalLiteral"
        || name == "uncheckedLiteral"
    _ -> False

-- | Build the expression, e.g.: safePositiveIntegerLiteral @N e
makeSafeLiteral :: HsExpr GhcPs -> Integer -> HsExpr GhcPs
makeSafeLiteral expr value = fullApp
 where
  funcName
    | value >= 0 = "safePositiveIntegerLiteral"
    | otherwise = "safeNegativeIntegerLiteral"
  funcRdrName = mkRdrUnqual (mkVarOcc funcName)
  funcVar = noLocA (HsVar noExtField (noLocA funcRdrName))
  tyLit = HsNumTy NoSourceText (abs value)
#if MIN_VERSION_ghc(9,10,0)
  typeArg = HsWC noExtField (noLocA (HsTyLit noExtField tyLit))
  withTypeApp = HsAppType noAnn funcVar typeArg
  fullApp = HsApp noExtField (noLocA withTypeApp) (noLocA expr)
#else
  typeArg = HsWC noExtField (noLocA (HsTyLit noExtField tyLit))
  atToken = L NoTokenLoc (HsTok @"@")
  withTypeApp = HsAppType noExtField funcVar atToken typeArg
  fullApp = HsApp noAnn (noLocA withTypeApp) (noLocA expr)
#endif

{- | Build the expression for rational literals, e.g.:
safePositiveRationalLiteral @"3.14" @314 @100 (3.14)
-}
makeSafeRationalLiteral :: HsExpr GhcPs -> Rational -> HsExpr GhcPs
makeSafeRationalLiteral expr rational = fullApp
 where
  funcName
    | rational >= 0 = "safePositiveRationalLiteral"
    | otherwise = "safeNegativeRationalLiteral"
  funcRdrName = mkRdrUnqual (mkVarOcc funcName)
  funcVar = noLocA (HsVar noExtField (noLocA funcRdrName))

  -- Create the rational and get its string representation. This allows errors messages to
  -- show "3.14" instead of "314 % 100".
  stringRepr = RatioExtra.showFixedPoint rational

  -- Type-level literals
  strTyLit = HsStrTy NoSourceText (mkFastString stringRepr)
  numTyLit = HsNumTy NoSourceText (abs (Ratio.numerator rational))
  denTyLit = HsNumTy NoSourceText (abs (Ratio.denominator rational))
#if MIN_VERSION_ghc(9,10,0)
  strTypeArg = HsWC noExtField (noLocA (HsTyLit noExtField strTyLit))
  numTypeArg = HsWC noExtField (noLocA (HsTyLit noExtField numTyLit))
  denTypeArg = HsWC noExtField (noLocA (HsTyLit noExtField denTyLit))
  withStrTypeApp = HsAppType noAnn funcVar strTypeArg
  withNumTypeApp = HsAppType noAnn (noLocA withStrTypeApp) numTypeArg
  withAllTypeApps = HsAppType noAnn (noLocA withNumTypeApp) denTypeArg
  fullApp = HsApp noExtField (noLocA withAllTypeApps) (noLocA expr)
#else
  strTypeArg = HsWC noExtField (noLocA (HsTyLit noExtField strTyLit))
  numTypeArg = HsWC noExtField (noLocA (HsTyLit noExtField numTyLit))
  denTypeArg = HsWC noExtField (noLocA (HsTyLit noExtField denTyLit))
  atToken = L NoTokenLoc (HsTok @"@")
  withStrTypeApp = HsAppType noExtField funcVar atToken strTypeArg
  withNumTypeApp = HsAppType noExtField (noLocA withStrTypeApp) atToken numTypeArg
  withAllTypeApps = HsAppType noExtField (noLocA withNumTypeApp) atToken denTypeArg
  fullApp = HsApp noAnn (noLocA withAllTypeApps) (noLocA expr)
#endif
