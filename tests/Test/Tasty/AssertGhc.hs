{-# LANGUAGE CPP #-}

module Test.Tasty.AssertGhc where

import Prelude

import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.Exit
import System.FilePath ((</>), takeDirectory)
import System.IO
import System.IO.Temp
import System.Process
import Test.Tasty (TestTree, askOption)
import Test.Tasty.HUnit
import Test.Tasty.Options
import Text.Read (readMaybe)

data Expected = ExpectFailure [String] | ExpectSuccess

-- | Option to enable debug output of GHC error messages
newtype DebugGhc = DebugGhc Bool
  deriving (Show, Read)

instance IsOption DebugGhc where
  defaultValue = DebugGhc False
  parseValue = fmap DebugGhc . readMaybe
  optionName = return "debug-ghc"
  optionHelp = return "Print full GHC output for error test cases"
  optionCLParser = flagCLParser Nothing (DebugGhc True)

testCaseGhc :: String -> String -> Expected -> TestTree
testCaseGhc name source expected =
  askOption $ \(DebugGhc debugGhc) ->
    testCaseInfo name $ do
      debugOutput <- assertGhc source expected
      if debugGhc then return debugOutput else return ""

{- | Assert that a Haskell code snippet fails to compile with expected error messages
Returns the GHC output for display in test results if debug flag is set
-}
assertGhc :: String -> Expected -> IO String
assertGhc source expected = do
  -- XXX: This will pick the wrong GHC if the HC environment variable (as seen on CI)
  --      isn't set and the test suite is compiled with a GHC compiler other than the
  --      system's default.
  hc <- fromMaybe "ghc" <$> lookupEnv "HC"
  packageDb <- findPackageDb
  clashNumsPackageId <- maybe (pure Nothing) findClashNumsPackageId packageDb
  let
    packageDbArgs = maybe [] (\db -> ["-package-db", db]) packageDb
    packageArgs =
      [ "-package"
      , "safe-literals"
      ]
        ++ maybe [] (\pkgId -> ["-package-id", pkgId]) clashNumsPackageId
  withSystemTempFile "ShouldError.hs" $ \tempFile tempHandle -> do
    -- Write source with proper Main module structure
    hPutStr tempHandle "module Main where\n"
    hPutStr tempHandle source
    hPutStr tempHandle "\nmain :: IO ()\nmain = return ()\n"
    hClose tempHandle
    (exitCode, _, stderrOutput) <-
      readProcessWithExitCode
        hc
        ( [ "-XCPP"
        , "-XDataKinds"
        , "-XTypeOperators"
        , "-XTypeApplications"
        , "-XTypeFamilies"
        , "-XUndecidableInstances"
        , "-XNoStarIsType"
        , "-XNoImplicitPrelude"
        , "-fno-code"
        , "-fplugin=GHC.TypeLits.KnownNat.Solver"
        , "-fplugin=GHC.TypeLits.Normalise"
        , "-fplugin=GHC.TypeLits.Extra.Solver"
        , "-fplugin=SafeLiterals"
        ]
          ++ packageDbArgs
          ++ packageArgs
          ++ [tempFile]
        )
        ""
    case (exitCode, expected) of
      (ExitSuccess, ExpectSuccess) ->
        return ""
      (ExitSuccess, ExpectFailure _) ->
        assertFailure "Expected compilation to fail but it succeeded" >> return ""
      (ExitFailure _, ExpectSuccess) ->
        assertFailure ("Expected compilation to succeed but it failed with error:\n" ++ stderrOutput)
          >> return ""
      (ExitFailure _, ExpectFailure expectedErrors) ->
        let cleanedStderr = removeProblemChars stderrOutput
            cleanedExpected = map removeProblemChars expectedErrors
         in if all (`isInfixOf` cleanedStderr) cleanedExpected
              then return stderrOutput
              else do
                _ <-
                  assertFailure $
                    "Error message mismatch:\n"
                      ++ "Expected substrings: "
                      ++ show expectedErrors
                      ++ "\n"
                      ++ "Actual output:\n"
                      ++ stderrOutput
                return stderrOutput

{- | Remove problematic characters that vary depending on locale
The kind and amount of quotes in GHC error messages changes depending on
whether or not our locale supports unicode.
-}
removeProblemChars :: String -> String
removeProblemChars = filter (`notElem` problemChars)
 where
  problemChars = "‘’`'"

findPackageDb :: IO (Maybe FilePath)
findPackageDb = do
  cwd <- getCurrentDirectory
  findIn cwd
 where
  findIn dir = do
    let base = dir </> "dist-newstyle" </> "packagedb"
    exists <- doesDirectoryExist base
    if exists
      then do
        entries <- listDirectory base
        let ghcDirs = sort (filter (isPrefixOf "ghc-") entries)
        case reverse ghcDirs of
          (latest : _) -> pure (Just (base </> latest))
          [] -> pure Nothing
      else do
        let parent = takeDirectory dir
        if parent == dir then pure Nothing else findIn parent

findClashNumsPackageId :: FilePath -> IO (Maybe String)
findClashNumsPackageId packageDb = do
  entries <- listDirectory packageDb
  let confs = filter (isSuffixOf "-clash-nums.conf") entries
  case confs of
    (conf : _) -> do
      contents <- readFile (packageDb </> conf)
      pure (extractField "id:" contents)
    [] -> pure Nothing
 where
  extractField prefix =
    listToMaybe
      . map (dropWhile isSpace . drop (length prefix))
      . filter (isPrefixOf prefix)
      . lines
