module Main where

import Data.Proxy
import Test.Tasty
import Test.Tasty.AssertGhc (DebugGhc (..))
import Test.Tasty.Options
import Prelude

import Tests.Integer qualified
import Tests.Rational qualified

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Tests.Integer.tests
    , Tests.Rational.tests
    ]

main :: IO ()
main = defaultMainWithIngredients ingredients tests
 where
  ingredients = includingOptions [Option (Proxy :: Proxy DebugGhc)] : defaultIngredients
