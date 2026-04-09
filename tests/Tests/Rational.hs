module Tests.Rational (tests) where

import Test.Tasty (TestTree, testGroup)

import Tests.Rational.Case qualified
import Tests.Rational.Fixed qualified
import Tests.Rational.FunctionPattern qualified
import Tests.Rational.Polymorphic qualified
import Tests.Rational.Ratio qualified

tests :: TestTree
tests =
  testGroup
    "Rational"
    [ Tests.Rational.Fixed.tests
    , Tests.Rational.Case.tests
    , Tests.Rational.FunctionPattern.tests
    , Tests.Rational.Polymorphic.tests
    , Tests.Rational.Ratio.tests
    ]
