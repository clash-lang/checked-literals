module Tests.Rational.Ratio (tests) where

import Prelude

import Data.Ratio ((%))
import Data.Ratio.Extra (showFixedPoint)
import Test.Tasty (TestTree, testGroup)
import Tests.Common (toTestCases)

rMod :: String
rMod = "Data.Ratio; import Data.Word; import Data.Int"

{- FOURMOLU_DISABLE -}
tests :: TestTree
tests = testGroup "Ratio" $ toTestCases
  [ (rMod, "Ratio Word8", "0.0",                    [])
  , (rMod, "Ratio Word8", "1.0",                    [])
  , (rMod, "Ratio Word8", "-1.0",                   ["Literal -1.0 cannot by represented by Ratio Word8.", "Word8 cannot represent negative numbers."])
  , (rMod, "Ratio Word8", showFixedPoint (1 % 256), ["Literal 0.00390625 (1 % 256) cannot by represented by Word8.", "Word8 has bounds: [0 .. 255]."])
  , (rMod, "Ratio Word8", "0.1",                    [])
  , (rMod, "Ratio Word8", "0.01",                   [])
  , (rMod, "Ratio Word8", "0.001",                  ["Literal 0.001 (1 % 1000) cannot by represented by Word8."])
  , (rMod, "Ratio Int8", "0.0",                     [])
  , (rMod, "Ratio Int8", "1.0",                     [])
  , (rMod, "Ratio Int8", "-1.0",                    [])
  , (rMod, "Ratio Int8", showFixedPoint (1 % 128),  ["Literal 0.0078125 (1 % 128) cannot by represented by Int8.", "Int8 has bounds: [-128 .. 127]."])
  , (rMod, "Ratio Int8", "0.1",                     [])
  , (rMod, "Ratio Int8", "0.01",                    [])
  , (rMod, "Ratio Int8", "0.001",                   ["Literal 0.001 (1 % 1000) cannot by represented by Int8."])
  ]
{- FOURMOLU_ENABLE -}
