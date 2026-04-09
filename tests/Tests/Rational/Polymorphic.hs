module Tests.Rational.Polymorphic (tests) where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Tests.Common (toTestCases)

{- FOURMOLU_DISABLE -}
tests :: TestTree
tests = testGroup "Polymorphic" $ toTestCases
  [ ("Prelude", "Fractional a => a", "0.5",  ["Cannot check literal", "at compile time.", "target type", "not (fully) known"])
  , ("Prelude", "Fractional a => a", "-0.5", ["Cannot check literal", "at compile time.", "target type", "not (fully) known"])
  , ("Prelude", "Fractional a => a", "3.14", ["Cannot check literal", "at compile time.", "target type", "not (fully) known"])
  ]
{- FOURMOLU_ENABLE -}
