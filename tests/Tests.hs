module Tests (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Tests.Unsigned as Unsigned
import qualified Tests.Word as Word

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Word.tests
    , Unsigned.tests
    ]
