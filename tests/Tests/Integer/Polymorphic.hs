module Tests.Integer.Polymorphic (tests) where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Tests.Common (toTestCases)

{- FOURMOLU_DISABLE -}
tests :: TestTree
tests = testGroup "Polymorphic" $ toTestCases
  [ ("Prelude", "Num a => a", "0",  ["Cannot check literal 0 at compile time.", "target type", "not (fully) known"])
  , ("Prelude", "Num a => a", "1",  ["Cannot check literal 1 at compile time.", "target type", "not (fully) known"])
  , ("Prelude", "Num a => a", "-1", ["Cannot check literal -1 at compile time.", "target type", "not (fully) known"])
  , ("Prelude", "Num a => a", "-5", ["Cannot check literal -5 at compile time.", "target type", "not (fully) known"])
  ]
{- FOURMOLU_ENABLE -}
