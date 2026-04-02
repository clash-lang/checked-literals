module Tests.Integer (tests) where

import Test.Tasty (TestTree, testGroup)

import Tests.Integer.Case qualified
import Tests.Integer.Fixed qualified
import Tests.Integer.FunctionPattern qualified
import Tests.Integer.Int qualified
import Tests.Integer.Signed qualified
import Tests.Integer.Unsigned qualified
import Tests.Integer.Word qualified

tests :: TestTree
tests =
  testGroup
    "Integer"
    [ Tests.Integer.Int.tests
    , Tests.Integer.Case.tests
    , Tests.Integer.FunctionPattern.tests
    , Tests.Integer.Signed.tests
    , Tests.Integer.Fixed.tests
    , Tests.Integer.Unsigned.tests
    , Tests.Integer.Word.tests
    ]
