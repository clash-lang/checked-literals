{-# LANGUAGE QuasiQuotes #-}

module Tests.Rational.FunctionPattern (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common (toFunctionPatternTestCases)

fMod :: String
fMod = "SafeLiterals.Nums.Fixed"

{- FOURMOLU_DISABLE -}
tests :: TestTree
tests = testGroup "FunctionPattern" $ toFunctionPatternTestCases
  [ (fMod, "UFixed 0 1", "0.5",  [])
  , (fMod, "UFixed 0 1", "0.75", ["Literal 0.75 cannot be represented exactly by Fixed", "The fractional part needs at least 2 bit(s)."])
  , (fMod, "UFixed 0 1", "(-0.5)", ["Literal -0.5 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "UFixed 0 1", "(uncheckedLiteral -> 0.75)", [])
  ]
{- FOURMOLU_ENABLE -}
