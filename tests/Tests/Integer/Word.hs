module Tests.Integer.Word (tests) where

import Prelude

import Data.Word
import Test.Tasty (TestTree, testGroup)
import Tests.Common (toTestCases)

{- FOURMOLU_DISABLE -}
tests :: TestTree
tests = testGroup "Word" $ toTestCases
  [ ("Data.Word", "Word",   show (minBound :: Word),                    [])
  , ("Data.Word", "Word",   show $ pred $ toInteger (minBound @Word),   ["Literal -1 is out of bounds.","Word has bounds: [0 .. 18446744073709551615]"])
  , ("Data.Word", "Word",   show (maxBound :: Word),                    [])
  , ("Data.Word", "Word",   show $ succ $ toInteger (maxBound @Word),   ["Literal 18446744073709551616 is out of bounds.", "Word has bounds: [0 .. 18446744073709551615]"])
  , ("Data.Word", "Word8",  show (minBound :: Word8),                   [])
  , ("Data.Word", "Word8",  show $ pred $ toInteger (minBound @Word8),  ["Literal -1 is out of bounds.", "Word8 has bounds: [0 .. 255]"])
  , ("Data.Word", "Word8",  show (maxBound :: Word8),                   [])
  , ("Data.Word", "Word8",  show $ succ $ toInteger (maxBound @Word8),  ["Literal 256 is out of bounds.", "Word8 has bounds: [0 .. 255]"])
  , ("Data.Word", "Word16", show (minBound :: Word16),                  [])
  , ("Data.Word", "Word16", show $ pred $ toInteger (minBound @Word16), ["Literal -1 is out of bounds.", "Word16 has bounds: [0 .. 65535]"])
  , ("Data.Word", "Word16", show (maxBound :: Word16),                  [])
  , ("Data.Word", "Word16", show $ succ $ toInteger (maxBound @Word16), ["Literal 65536 is out of bounds.", "Word16 has bounds: [0 .. 65535]"])
  , ("Data.Word", "Word32", show (minBound :: Word32),                  [])
  , ("Data.Word", "Word32", show $ pred $ toInteger (minBound @Word32), ["Literal -1 is out of bounds.", "Word32 has bounds: [0 .. 4294967295]"])
  , ("Data.Word", "Word32", show (maxBound :: Word32),                  [])
  , ("Data.Word", "Word32", show $ succ $ toInteger (maxBound @Word32), ["Literal 4294967296 is out of bounds.", "Word32 has bounds: [0 .. 4294967295]"])
  , ("Data.Word", "Word64", show (minBound :: Word64),                  [])
  , ("Data.Word", "Word64", show $ pred $ toInteger (minBound @Word64), ["Literal -1 is out of bounds.", "Word64 has bounds: [0 .. 18446744073709551615]"])
  , ("Data.Word", "Word64", show (maxBound :: Word64),                  [])
  , ("Data.Word", "Word64", show $ succ $ toInteger (maxBound @Word64), ["Literal 18446744073709551616 is out of bounds.", "Word64 has bounds: [0 .. 18446744073709551615]"])
  ]
{- FOURMOLU_ENABLE -}
