{-# LANGUAGE CPP #-}

module Test.Tasty.ShouldError where

import Prelude

import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import Test.Tasty.HUnit
import Test.Tasty.Options
import Text.Read (readMaybe)

-- | Option to enable debug output of GHC error messages
newtype DebugGhc = DebugGhc Bool
  deriving (Show, Read)

instance IsOption DebugGhc where
  defaultValue = DebugGhc False
  parseValue = fmap DebugGhc . readMaybe
  optionName = return "debug-ghc"
  optionHelp = return "Print full GHC output for error test cases"
  optionCLParser = flagCLParser Nothing (DebugGhc True)

{- | Assert that a Haskell code snippet fails to compile with expected error messages
Returns the GHC output for display in test results if debug flag is set
-}
assertCompileError :: Bool -> String -> [String] -> IO String
assertCompileError debugGhc source expectedErrors = do
  -- XXX: This will pick the wrong GHC if the HC environment variable (as seen on CI)
  --      isn't set and the test suite is compiled with a GHC compiler other than the
  --      system's default.
  hc <- fromMaybe "ghc" <$> lookupEnv "HC"
  withSystemTempFile "ShouldError.hs" $ \tempFile tempHandle -> do
    -- Write source with proper Main module structure
    hPutStr tempHandle "module Main where\n"
    hPutStr tempHandle source
    hPutStr tempHandle "\nmain :: IO ()\nmain = return ()\n"
    hClose tempHandle
    (exitCode, _, stderrOutput) <-
      readProcessWithExitCode
        hc
        [ "-XCPP"
        , "-XDataKinds"
        , "-XTypeOperators"
        , "-XTypeApplications"
        , "-XTypeFamilies"
        , "-XUndecidableInstances"
        , "-XNoStarIsType"
        , "-XNoImplicitPrelude"
        , "-fno-code"
        , "-fplugin"
        , "GHC.TypeLits.KnownNat.Solver"
        , "-fplugin"
        , "GHC.TypeLits.Normalise"
        , "-fplugin"
        , "GHC.TypeLits.Extra.Solver"
        , "-fplugin"
        , "SafeLiterals"
        , tempFile
        ]
        ""
    case exitCode of
      ExitSuccess -> assertFailure "Expected compilation to fail but it succeeded" >> return ""
      ExitFailure _ ->
        let cleanedStderr = removeProblemChars stderrOutput
            cleanedExpected = map removeProblemChars expectedErrors
         in if all (`isInfixOf` cleanedStderr) cleanedExpected
              then return (if debugGhc then stderrOutput else "")
              else do
                _ <-
                  assertFailure $
                    "Error message mismatch:\n"
                      ++ "Expected substrings: "
                      ++ show expectedErrors
                      ++ "\n"
                      ++ "Actual output:\n"
                      ++ stderrOutput
                return (if debugGhc then stderrOutput else "")

{- | Remove problematic characters that vary depending on locale
The kind and amount of quotes in GHC error messages changes depending on
whether or not our locale supports unicode.
-}
removeProblemChars :: String -> String
removeProblemChars = filter (`notElem` problemChars)
 where
  problemChars = "‘’`'"
