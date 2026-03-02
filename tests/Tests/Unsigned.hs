{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tests.Unsigned (tests) where

import Prelude

import GHC.TypeLits (type (<=?), type (<=), type (^), type (-), Nat, KnownNat, natVal)
import GHC.TypeError (Assert)
import GHC.TypeLits.Extra (CLogWZ)
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup, askOption)
import Test.Tasty.HUnit (testCase, testCaseInfo, (@?=))
import Test.Tasty.ShouldError (DebugGhc (..), assertCompileError)

-- | Unsigned integer type parameterized by bit width
data Unsigned (n :: Nat) = Unsigned Natural
  deriving (Eq)

instance Show (Unsigned n) where
  show (Unsigned x) = show x

instance KnownNat n => Num (Unsigned n) where
  fromInteger x = Unsigned (fromInteger x `mod` (2 ^ natVal (undefined :: Unsigned n)))
  Unsigned a + Unsigned b = fromInteger (toInteger (a + b))
  Unsigned a * Unsigned b = fromInteger (toInteger (a * b))
  Unsigned a - Unsigned b = fromInteger (toInteger a - toInteger b)
  abs = id
  signum (Unsigned 0) = Unsigned 0
  signum _ = Unsigned 1
  negate = error "Cannot negate Unsigned"

-- | Instance for safe positive integer literals on Unsigned types
instance
  ( Assert (CLogWZ 2 lit 0 <=? n)
      (PositiveUnsignedError lit (Unsigned n) ((2 ^ n) - 1))
  ) =>
   SafePositiveIntegerLiteral lit (Unsigned n)

-- | Instance for safe negative integer literals on Unsigned types (always errors)
instance
  ( NegativeUnsignedError lit (Unsigned n) ((2 ^ n) - 1)
  ) =>
  SafeNegativeIntegerLiteral lit (Unsigned n)

tests :: TestTree
tests =
  testGroup
    "Unsigned"
    [ testGroup
        "should-work"
        [ testCase "Unsigned 8 ~ 0" (show testU8_0 @?= "0")
        , testCase "Unsigned 8 ~ 255" (show testU8_255 @?= "255")
        , testCase "Unsigned 8 ~ maxBound" (testU8_max @?= Unsigned 255)
        , testCase "Unsigned 4 ~ 0" (show testU4_0 @?= "0")
        , testCase "Unsigned 4 ~ 15" (show testU4_15 @?= "15")
        , testCase "Unsigned 4 ~ maxBound" (testU4_max @?= Unsigned 15)
        , testCase "Unsigned 16 ~ 0" (show testU16_0 @?= "0")
        , testCase "Unsigned 16 ~ 65535" (show testU16_65535 @?= "65535")
        , testCase "Unsigned 16 ~ maxBound" (testU16_max @?= Unsigned 65535)
        , testCase "Unsigned 32 ~ 0" (show testU32_0 @?= "0")
        , testCase "Unsigned 32 ~ maxBound" (testU32_max @?= Unsigned 4294967295)
        , testCase "Unsigned 1 ~ 0" (show testU1_0 @?= "0")
        , testCase "Unsigned 1 ~ 1" (show testU1_1 @?= "1")
        , testCase "Polymorphic 0 (3 <= n) ~ 0" (show (testPoly3_0 @8) @?= "0")
        , testCase "Polymorphic 1 (3 <= n) ~ 1" (show (testPoly3_1 @8) @?= "1")
        , testCase "Polymorphic 2 (3 <= n) ~ 2" (show (testPoly3_2 @8) @?= "2")
        , testCase "Polymorphic 7 (3 <= n) ~ 7" (show (testPoly3_7 @8) @?= "7")
        , testCase "Polymorphic (4 <= n) ~ 7" (show (testPoly4 @8) @?= "7")
        , testCase "Polymorphic (8 <= n) ~ 255" (show (testPoly8 @16) @?= "255")
        ]
    , testGroup
        "should-error"
        [ askOption $ \(DebugGhc debug) ->
            testCaseInfo "Unsigned 8 out of bounds" $
              assertCompileError
                debug
                "import Prelude\nimport GHC.TypeLits\nimport GHC.TypeError\nimport Numeric.Natural\nimport SafeLiterals\ndata Unsigned (n :: Nat) = Unsigned Natural\ninstance (Assert (lit <=? ((2 ^ n) - 1)) (PositiveUnsignedError lit (Unsigned n) ((2 ^ n) - 1))) => SafePositiveIntegerLiteral lit (Unsigned n)\ntest :: Unsigned 8\ntest = 256"
                [ "Literal 256 is out of bounds"
                , "has bounds: [0 .. 255]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Unsigned 4 out of bounds" $
              assertCompileError
                debug
                "import Prelude\nimport GHC.TypeLits\nimport GHC.TypeError\nimport Numeric.Natural\nimport SafeLiterals\ndata Unsigned (n :: Nat) = Unsigned Natural\ninstance (Assert (lit <=? ((2 ^ n) - 1)) (PositiveUnsignedError lit (Unsigned n) ((2 ^ n) - 1))) => SafePositiveIntegerLiteral lit (Unsigned n)\ntest :: Unsigned 4\ntest = 16"
                [ "Literal 16 is out of bounds"
                , "has bounds: [0 .. 15]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Unsigned 16 out of bounds" $
              assertCompileError
                debug
                "import Prelude\nimport GHC.TypeLits\nimport GHC.TypeError\nimport Numeric.Natural\nimport SafeLiterals\ndata Unsigned (n :: Nat) = Unsigned Natural\ninstance (Assert (lit <=? ((2 ^ n) - 1)) (PositiveUnsignedError lit (Unsigned n) ((2 ^ n) - 1))) => SafePositiveIntegerLiteral lit (Unsigned n)\ntest :: Unsigned 16\ntest = 65536"
                [ "Literal 65536 is out of bounds"
                , "has bounds: [0 .. 65535]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Unsigned 1 out of bounds" $
              assertCompileError
                debug
                "import Prelude\nimport GHC.TypeLits\nimport GHC.TypeError\nimport Numeric.Natural\nimport SafeLiterals\ndata Unsigned (n :: Nat) = Unsigned Natural\ninstance (Assert (lit <=? ((2 ^ n) - 1)) (PositiveUnsignedError lit (Unsigned n) ((2 ^ n) - 1))) => SafePositiveIntegerLiteral lit (Unsigned n)\ntest :: Unsigned 1\ntest = 2"
                [ "Literal 2 is out of bounds"
                , "has bounds: [0 .. 1]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Unsigned 8 negative" $
              assertCompileError
                debug
                "import Prelude\nimport GHC.TypeLits\nimport GHC.TypeError\nimport Numeric.Natural\nimport SafeLiterals\ndata Unsigned (n :: Nat) = Unsigned Natural\ninstance (NegativeUnsignedError lit (Unsigned n) ((2 ^ n) - 1)) => SafeNegativeIntegerLiteral lit (Unsigned n)\ntest :: Unsigned 8\ntest = -1"
                [ "Negative literal -1 is out of bounds"
                , "has bounds: [0 .. 255]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Unsigned 4 negative" $
              assertCompileError
                debug
                "import Prelude\nimport GHC.TypeLits\nimport GHC.TypeError\nimport Numeric.Natural\nimport SafeLiterals\ndata Unsigned (n :: Nat) = Unsigned Natural\ninstance (NegativeUnsignedError lit (Unsigned n) ((2 ^ n) - 1)) => SafeNegativeIntegerLiteral lit (Unsigned n)\ntest :: Unsigned 4\ntest = -5"
                [ "Negative literal -5 is out of bounds"
                , "has bounds: [0 .. 15]"
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        , askOption $ \(DebugGhc debug) ->
            testCaseInfo "Polymorphic (2 <= n) out of bounds" $
              assertCompileError
                debug
                "import Prelude\nimport GHC.TypeLits\nimport GHC.TypeError\nimport Numeric.Natural\nimport SafeLiterals\ndata Unsigned (n :: Nat) = Unsigned Natural\ninstance (Assert (lit <=? ((2 ^ n) - 1)) (PositiveUnsignedError lit (Unsigned n) ((2 ^ n) - 1))) => SafePositiveIntegerLiteral lit (Unsigned n)\ntest :: (2 <= n) => Unsigned n\ntest = 7"
                [ "Literal 7 is out of bounds"
                , "has bounds: [0 .."
                , "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check"
                ]
        ]
    , testGroup
        "uncheckedLiteral"
        [ testCase "Unsigned 8 bypass with uncheckedLiteral" (show testUncheckedU8 @?= "0")
        , testCase "Unsigned 4 bypass with uncheckedLiteral" (show testUncheckedU4 @?= "0")
        ]
    ]

-- Valid tests
testU8_0 :: Unsigned 8
testU8_0 = 0

testU8_255 :: Unsigned 8
testU8_255 = 255

testU8_max :: Unsigned 8
testU8_max = 255

testU4_0 :: Unsigned 4
testU4_0 = 0

testU4_15 :: Unsigned 4
testU4_15 = 15

testU4_max :: Unsigned 4
testU4_max = 15

testU16_0 :: Unsigned 16
testU16_0 = 0

testU16_65535 :: Unsigned 16
testU16_65535 = 65535

testU16_max :: Unsigned 16
testU16_max = 65535

testU32_0 :: Unsigned 32
testU32_0 = 0

testU32_max :: Unsigned 32
testU32_max = 4294967295

testU1_0 :: Unsigned 1
testU1_0 = 0

testU1_1 :: Unsigned 1
testU1_1 = 1

-- uncheckedLiteral tests (these will overflow but demonstrate the bypass)
testUncheckedU8 :: Unsigned 8
testUncheckedU8 = uncheckedLiteral 256

testUncheckedU4 :: Unsigned 4
testUncheckedU4 = uncheckedLiteral 16

-- Polymorphic tests with constraints
testPoly3_0 :: (3 <= n, KnownNat n) => Unsigned n
testPoly3_0 = 0

testPoly3_1 :: (3 <= n, KnownNat n) => Unsigned n
testPoly3_1 = 1

testPoly3_2 :: (3 <= n, KnownNat n) => Unsigned n
testPoly3_2 = 2

testPoly3_7 :: (3 <= n, KnownNat n) => Unsigned n
testPoly3_7 = 7

testPoly4 :: (4 <= n, KnownNat n) => Unsigned n
testPoly4 = 7

testPoly8 :: (8 <= n, KnownNat n) => Unsigned n
testPoly8 = 255
