module Main where

import Data.Proxy
import Prelude
import Test.Tasty
import Test.Tasty.Options

import Test.Tasty.ShouldError (DebugGhc (..))

import Tests qualified

main :: IO ()
main = defaultMainWithIngredients ingredients Tests.tests
 where
  ingredients = includingOptions [Option (Proxy :: Proxy DebugGhc)] : defaultIngredients
