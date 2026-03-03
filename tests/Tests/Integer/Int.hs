module Tests.Integer.Int (tests) where

import Prelude

import Data.Int
import Test.Tasty (TestTree, testGroup)
import Tests.Common (toTestCases)

{- FOURMOLU_DISABLE -}
tests :: TestTree
tests = testGroup "Int" $ toTestCases
  [ ("Data.Int", "Int",   "0",                                       [])
  , ("Data.Int", "Int",   show (minBound :: Int),                    [])
  , ("Data.Int", "Int",   show $ pred $ toInteger (minBound @Int),   ["Literal -9223372036854775809 is out of bounds.",  "Int has bounds: [-9223372036854775808 .. 9223372036854775807]"])
  , ("Data.Int", "Int",   show (maxBound :: Int),                    [])
  , ("Data.Int", "Int",   show $ succ $ toInteger (maxBound @Int),   ["Literal 9223372036854775808 is out of bounds.", "Int has bounds: [-9223372036854775808 .. 9223372036854775807]"])
  , ("Data.Int", "Int8",  "0",                                       [])
  , ("Data.Int", "Int8",  show (minBound :: Int8),                   [])
  , ("Data.Int", "Int8",  show $ pred $ toInteger (minBound @Int8),  ["Literal -129 is out of bounds.", "Int8 has bounds: [-128 .. 127]"])
  , ("Data.Int", "Int8",  show (maxBound :: Int8),                   [])
  , ("Data.Int", "Int8",  show $ succ $ toInteger (maxBound @Int8),  ["Literal 128 is out of bounds.", "Int8 has bounds: [-128 .. 127]"])
  , ("Data.Int", "Int16", "0",                                       [])
  , ("Data.Int", "Int16", show (minBound :: Int16),                  [])
  , ("Data.Int", "Int16", show $ pred $ toInteger (minBound @Int16), ["Literal -32769 is out of bounds.", "Int16 has bounds: [-32768 .. 32767]"])
  , ("Data.Int", "Int16", show (maxBound :: Int16),                  [])
  , ("Data.Int", "Int16", show $ succ $ toInteger (maxBound @Int16), ["Literal 32768 is out of bounds.", "Int16 has bounds: [-32768 .. 32767]"])
  , ("Data.Int", "Int32", "0",                                       [])
  , ("Data.Int", "Int32", show (minBound :: Int32),                  [])
  , ("Data.Int", "Int32", show $ pred $ toInteger (minBound @Int32), ["Literal -2147483649 is out of bounds.", "Int32 has bounds: [-2147483648 .. 2147483647]"])
  , ("Data.Int", "Int32", show (maxBound :: Int32),                  [])
  , ("Data.Int", "Int32", show $ succ $ toInteger (maxBound @Int32), ["Literal 2147483648 is out of bounds.", "Int32 has bounds: [-2147483648 .. 2147483647]"])
  , ("Data.Int", "Int64", "0",                                       [])
  , ("Data.Int", "Int64", show (minBound :: Int64),                  [])
  , ("Data.Int", "Int64", show $ pred $ toInteger (minBound @Int64), ["Literal -9223372036854775809 is out of bounds.", "Int64 has bounds: [-9223372036854775808 .. 9223372036854775807]"])
  , ("Data.Int", "Int64", show (maxBound :: Int64),                  [])
  , ("Data.Int", "Int64", show $ succ $ toInteger (maxBound @Int64), ["Literal 9223372036854775808 is out of bounds.", "Int64 has bounds: [-9223372036854775808 .. 9223372036854775807]"])
  ]
{- FOURMOLU_ENABLE -}
