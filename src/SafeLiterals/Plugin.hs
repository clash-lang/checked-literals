{-# LANGUAGE ViewPatterns #-}

module SafeLiterals.Plugin (plugin) where

import Prelude
import GHC.Hs

import Control.Monad.State (State, runState, put)
import Data.Generics (Data, extM, gmapM)
import GHC.Plugins hiding ((<>))
import GHC.Types.SourceText (il_value, SourceText (NoSourceText))

-- | Type alias for transformation state: tracks whether any changes occurred
type TransformM = State Bool

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
  -- liftIO $ putStrLn (showPprUnsafe (hpm_module transformedModule))
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

transformData :: Data a => a -> TransformM a
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

{- | Transform a located expression using top-down traversal.
-}
transformLHsExpr :: LHsExpr GhcPs -> TransformM (LHsExpr GhcPs)
transformLHsExpr lexpr@(L loc expr) = case expr of
  -- Check if this is an application to our safe literal functions. If so, stop recursing
  -- to avoid double transformation.
  HsApp _ fun _ | isSafeLiteralApp (unLoc fun) -> return lexpr

  -- Handle negation: detect (negate literal) patterns
  NegApp _ (L _ (HsOverLit _ OverLit{ol_val = HsIntegral intLit})) _ -> do
    put True -- Mark that we performed a change
    let
      value = il_value intLit
      transformedExpr = makeSafeLiteral "safeNegativeIntegerLiteral" expr value
    return (L loc transformedExpr)

  -- Transform positive numeric literals
  HsOverLit _ OverLit{ol_val = HsIntegral intLit} -> do
    put True -- Mark that we performed a change
    let value = il_value intLit
    if value >= 0
      then return $ L loc $ makeSafeLiteral "safePositiveIntegerLiteral" expr value
      else return $ L loc $ makeSafeLiteral "safeNegativeIntegerLiteral" expr value

  -- For all other expressions, recurse into children (top-down)
  _ -> L loc <$> gmapM transformData expr

-- | Check if an expression is an application to one of our safe literal functions
isSafeLiteralApp :: HsExpr GhcPs -> Bool
isSafeLiteralApp expr = case expr of
  -- Direct reference to safe literal function
  HsVar _ (L _ rdrName) -> isSafeLiteralName rdrName
  -- Type application to safe literal function, e.g.: safePositiveIntegerLiteral @N
  HsAppType _ funExpr _ -> isSafeLiteralApp (unLoc funExpr)
  _ -> False

-- | Check if a name is one of our safe literal functions or uncheckedLiteral
isSafeLiteralName :: RdrName -> Bool
isSafeLiteralName rdrName =
  case rdrName of
    Unqual (occNameString -> name) ->
      name == "safePositiveIntegerLiteral"
        || name == "safeNegativeIntegerLiteral"
        || name == "uncheckedLiteral"
    _ -> False

{- | Build the expression, e.g.: safePositiveIntegerLiteral @N e
-}
makeSafeLiteral :: String -> HsExpr GhcPs -> Integer -> HsExpr GhcPs
makeSafeLiteral funcName expr value = fullApp
 where
  funcRdrName = mkRdrUnqual (mkVarOcc funcName)
  funcVar = noLocA (HsVar noExtField (noLocA funcRdrName))
  tyLit = HsNumTy NoSourceText (abs value)
  typeArg = HsWC noExtField (noLocA (HsTyLit noExtField tyLit))
  withTypeApp = HsAppType noAnn funcVar typeArg
  fullApp = HsApp noExtField (noLocA withTypeApp) (noLocA expr)
