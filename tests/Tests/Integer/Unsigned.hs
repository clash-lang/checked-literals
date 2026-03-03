module Tests.Integer.Unsigned where

import Prelude

import Test.Tasty (TestTree, testGroup)
import Tests.Common (toTestCases)

uMod :: String
uMod = "SafeLiterals.Nums.Unsigned"

{- FOURMOLU_DISABLE -}
tests :: TestTree
tests = testGroup "Unsigned" $ toTestCases
  [ (uMod, "Unsigned 0", "0",                          [])
  , (uMod, "Unsigned 0", "-1",                         ["Literal -1 is out of bounds.", "Unsigned 0 has bounds: [0 .. 0]."])
  , (uMod, "Unsigned 0", "1",                          ["Literal 1 is (potentially) out of bounds.", "Unsigned 0 has bounds: [0 .. 0]."])
  , (uMod, "Unsigned 1", "0",                          [])
  , (uMod, "Unsigned 1", "-1",                         ["Literal -1 is out of bounds.", "Unsigned 1 has bounds: [0 .. 1]."])
  , (uMod, "Unsigned 1", "1",                          [])
  , (uMod, "Unsigned 1", "2",                          ["Literal 2 is (potentially) out of bounds.", "Unsigned 1 has bounds: [0 .. 1]."])
  , (uMod, "Unsigned 2", "0",                          [])
  , (uMod, "Unsigned 2", "-1",                         ["Literal -1 is out of bounds.", "Unsigned 2 has bounds: [0 .. 3]."])
  , (uMod, "Unsigned 2", "1",                          [])
  , (uMod, "Unsigned 2", "2",                          [])
  , (uMod, "Unsigned 2", "3",                          [])
  , (uMod, "Unsigned 2", "4",                          ["Literal 4 is (potentially) out of bounds.", "Unsigned 2 has bounds: [0 .. 3]."])
  , (uMod, "(KnownNat n) => Unsigned n", "0",          [])
  , (uMod, "(KnownNat n) => Unsigned n", "1",          ["Literal 1 is (potentially) out of bounds.", "Unsigned n has bounds: [0 .. (2 ^ n) - 1]", "Possible fix: add a constraint: 1 <= n."])
  , (uMod, "(KnownNat n) => Unsigned n", "-1",         ["Literal -1 is out of bounds.", "Unsigned n has bounds: [0 .. (2 ^ n) - 1]."])
  , (uMod, "(KnownNat n, 1 <= n) => Unsigned n", "0",  [])
  , (uMod, "(KnownNat n, 1 <= n) => Unsigned n", "1",  [])
  , (uMod, "(KnownNat n, 1 <= n) => Unsigned n", "-1", ["Literal -1 is out of bounds.", "Unsigned n has bounds: [0 .. (2 ^ n) - 1]."])
  , (uMod, "(KnownNat n, 2 <= n) => Unsigned n", "0",  [])
  , (uMod, "(KnownNat n, 2 <= n) => Unsigned n", "1",  [])
  , (uMod, "(KnownNat n, 2 <= n) => Unsigned n", "-1", ["Literal -1 is out of bounds.", "Unsigned n has bounds: [0 .. (2 ^ n) - 1]."])
  ]
{- FOURMOLU_ENABLE -}
