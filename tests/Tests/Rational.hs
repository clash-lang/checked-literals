module Tests.Rational (tests) where

import Test.Tasty (TestTree, testGroup)

import Tests.Rational.Fixed qualified
import Tests.Rational.Ratio qualified

tests :: TestTree
tests =
  testGroup
    "Rational"
    [ Tests.Rational.Fixed.tests
    , Tests.Rational.Ratio.tests
    ]
