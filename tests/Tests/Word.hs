module Tests.Word (tests) where

import Prelude

import Data.Word
import Test.Tasty (TestTree, testGroup, askOption)
import Test.Tasty.HUnit (testCase, testCaseInfo, (@?=))
import Test.Tasty.ShouldError (DebugGhc (..), assertCompileError)

tests :: TestTree
tests =
  testGroup
    "Word"
    [ testGroup
        "should-work"
        [ testCase "Word ~ 0" (show testWord0 @?= "0")
        , testCase "Word ~ 255" (show testWord255 @?= "255")
        , testCase "Word ~ maxBound" (show testWordMax @?= show (maxBound :: Word))
        , testCase "Word8 ~ 0" (show testWord8_0 @?= "0")
        , testCase "Word8 ~ 255" (show testWord8_255 @?= "255")
        , testCase "Word8 ~ maxBound" (testWord8_max @?= 255)
        , testCase "Word16 ~ 0" (show testWord16_0 @?= "0")
        , testCase "Word16 ~ 65535" (show testWord16_65535 @?= "65535")
        , testCase "Word16 ~ maxBound" (testWord16_max @?= 65535)
        , testCase "Word32 ~ 0" (show testWord32_0 @?= "0")
        , testCase "Word32 ~ 4294967295" (show testWord32_max @?= "4294967295")
        , testCase "Word64 ~ 0" (show testWord64_0 @?= "0")
        , testCase "Word64 ~ large value" (show testWord64_large @?= "18446744073709551615")
        ]
    , testGroup
        "should-error"
        [ askOption $ \(DebugGhc debug) ->
            testCaseInfo "Word8 out of bounds" $
              assertCompileError
                debug
                "import Prelude\nimport Data.Word\ntest :: Word8\ntest = 256"
                [ "Literal 256 is out of bounds"
                , "Word8 has bounds: [0 .. 255]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Word16 out of bounds" $
              assertCompileError
                debug
                "import Prelude\nimport Data.Word\ntest :: Word16\ntest = 65536"
                [ "Literal 65536 is out of bounds"
                , "Word16 has bounds: [0 .. 65535]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Word32 out of bounds" $
              assertCompileError
                debug
                "import Prelude\nimport Data.Word\ntest :: Word32\ntest = 4294967296"
                [ "Literal 4294967296 is out of bounds"
                , "Word32 has bounds: [0 .. 4294967295]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Word8 negative" $
              assertCompileError
                debug
                "import Prelude\nimport Data.Word\ntest :: Word8\ntest = -1"
                [ "Negative literal -1 is out of bounds"
                , "Word8 has bounds: [0 .. 255]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Word16 negative" $
              assertCompileError
                debug
                "import Prelude\nimport Data.Word\ntest :: Word16\ntest = -100"
                [ "Negative literal -100 is out of bounds"
                , "Word16 has bounds: [0 .. 65535]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Word negative" $
              assertCompileError
                debug
                "import Prelude\ntest :: Word\ntest = -42"
                [ "Negative literal -42 is out of bounds"
                , "Word has bounds: [0 .."
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        ]
    , testGroup
        "uncheckedLiteral"
        [ testCase "Word8 bypass with uncheckedLiteral" (show testUncheckedWord8 @?= "0")
        , testCase "Word16 bypass with uncheckedLiteral" (show testUncheckedWord16 @?= "0")
        ]
    ]

-- Valid tests
testWord0 :: Word
testWord0 = 0

testWord255 :: Word
testWord255 = 255

testWordMax :: Word
testWordMax = 18446744073709551615

testWord8_0 :: Word8
testWord8_0 = 0

testWord8_255 :: Word8
testWord8_255 = 255

testWord8_max :: Word8
testWord8_max = 255

testWord16_0 :: Word16
testWord16_0 = 0

testWord16_65535 :: Word16
testWord16_65535 = 65535

testWord16_max :: Word16
testWord16_max = 65535

testWord32_0 :: Word32
testWord32_0 = 0

testWord32_max :: Word32
testWord32_max = 4294967295

testWord64_0 :: Word64
testWord64_0 = 0

testWord64_large :: Word64
testWord64_large = 18446744073709551615

-- uncheckedLiteral tests (these will overflow but demonstrate the bypass)
testUncheckedWord8 :: Word8
testUncheckedWord8 = uncheckedLiteral 256

testUncheckedWord16 :: Word16
testUncheckedWord16 = uncheckedLiteral 65536
