{-# LANGUAGE QuasiQuotes #-}

module Tests.Integer.FunctionPattern (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common (toFunctionPatternTestCases)

{- FOURMOLU_DISABLE -}
tests :: TestTree
tests = testGroup "FunctionPattern" $
  toFunctionPatternTestCases
    [ ("Data.Word", "Word8", "255", [])
    , ("Data.Word", "Word8", "256", ["Literal 256 is out of bounds.", "Word8 has bounds: [0 .. 255]."])
    , ("Data.Word", "Word8", "(-1)", ["Literal -1 is out of bounds.", "Word8 has bounds: [0 .. 255]."])
    , ("Data.Word", "Word8", "(uncheckedLiteral -> 256)", [])
    , ("Data.Int", "Int8", "127", [])
    , ("Data.Int", "Int8", "128", ["Literal 128 is out of bounds.", "Int8 has bounds: [-128 .. 127]."])
    ]
{- FOURMOLU_ENABLE -}
