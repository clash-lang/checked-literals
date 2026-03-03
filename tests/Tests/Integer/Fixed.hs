module Tests.Integer.Fixed where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Tests.Common (toTestCases)

fMod :: String
fMod = "SafeLiterals.Nums.Fixed"

{- FOURMOLU_DISABLE -}
tests :: TestTree
tests = testGroup "Fixed" $ toTestCases
  [ (fMod, "(KnownNat f) => UFixed 0 f", "0",                      [])
  , (fMod, "(KnownNat f) => UFixed 0 f", "-1",                     ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f) => UFixed 0 f", "1",                      ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s)."])
  , (fMod, "(KnownNat f) => UFixed 1 f", "0",                      [])
  , (fMod, "(KnownNat f) => UFixed 1 f", "-1",                     ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f) => UFixed 1 f", "1",                      [])
  , (fMod, "(KnownNat f) => UFixed 1 f", "2",                      ["Literal 2 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s)."])
  , (fMod, "(KnownNat f) => UFixed 2 f", "0",                      [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "-1",                     ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f) => UFixed 2 f", "1",                      [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "2",                      [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "3",                      [])
  , (fMod, "(KnownNat f) => UFixed 2 f", "4",                      ["Literal 4 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s)."])
  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "0",          [])
  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "1",          ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s).", "Possible fix: add a constraint: 1 <= n."])
  , (fMod, "(KnownNat f, KnownNat n) => UFixed n f", "-1",         ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => UFixed n f", "0",  [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => UFixed n f", "1",  [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => UFixed n f", "-1", ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => UFixed n f", "0",  [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => UFixed n f", "1",  [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => UFixed n f", "-1", ["Literal -1 is out of bounds, because UFixed cannot represent negative numbers."])

  , (fMod, "(KnownNat f) => SFixed 0 f", "0",                      [])
  , (fMod, "(KnownNat f) => SFixed 0 f", "-1",                     ["Literal -1 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 0 f", "1",                      ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 1 f", "0",                      [])
  , (fMod, "(KnownNat f) => SFixed 1 f", "-1",                     [])
  , (fMod, "(KnownNat f) => SFixed 1 f", "1",                      ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 1 f", "2",                      ["Literal 2 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 2 f", "0",                      [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "-1",                     [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "-2",                     [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "-3",                     ["Literal -3 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 2 f", "1",                      [])
  , (fMod, "(KnownNat f) => SFixed 2 f", "2",                      ["Literal 2 is (potentially) out of bounds.", "Note: integer part needs at least 3 bit(s), including sign bit."])
  , (fMod, "(KnownNat f) => SFixed 2 f", "4",                      ["Literal 4 is (potentially) out of bounds.", "Note: integer part needs at least 4 bit(s), including sign bit."])
  , (fMod, "(KnownNat f, KnownNat n) => SFixed n f", "0",          [])
  , (fMod, "(KnownNat f, KnownNat n) => SFixed n f", "1",          ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit.", "Possible fix: add a constraint: 2 <= n."])
  , (fMod, "(KnownNat f, KnownNat n) => SFixed n f", "-1",         ["Literal -1 is (potentially) out of bounds.", "Note: integer part needs at least 1 bit(s), including sign bit.", "Possible fix: add a constraint: 1 <= n."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => SFixed n f", "0",  [])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => SFixed n f", "1",  ["Literal 1 is (potentially) out of bounds.", "Note: integer part needs at least 2 bit(s), including sign bit.", "Possible fix: add a constraint: 2 <= n."])
  , (fMod, "(KnownNat f, KnownNat n, 1 <= n) => SFixed n f", "-1", [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => SFixed n f", "0",  [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => SFixed n f", "1",  [])
  , (fMod, "(KnownNat f, KnownNat n, 2 <= n) => SFixed n f", "-1", [])
  ]
{- FOURMOLU_ENABLE -}
