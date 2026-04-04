{-# LANGUAGE UndecidableInstances #-}

module CheckedLiterals.Nums.Unsigned (
  Unsigned (..),
) where

import CheckedLiterals (
  CheckedNegativeIntegerLiteral,
  CheckedPositiveIntegerLiteral,
  NegativeUnsignedError,
 )
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Proxy (Proxy (..))
import Data.Type.Bool (If)
import GHC.TypeError (Assert, ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeLits (KnownNat, Nat, natVal, type (+), type (-), type (<=?), type (^))
import GHC.TypeLits.Extra (CLog)

-- | Unsigned integer with @n@ bits
newtype Unsigned (n :: Nat) = Unsigned Integer
  deriving (Eq, Ord)

instance (KnownNat n) => Show (Unsigned n) where
  show (Unsigned i) = show i

instance (KnownNat n) => Bounded (Unsigned n) where
  minBound = Unsigned 0
  maxBound =
    let n = natVal (Proxy @n)
     in Unsigned (2 ^ n - 1)

instance (KnownNat n) => Num (Unsigned n) where
  Unsigned a + Unsigned b = fromInteger (a + b)
  Unsigned a * Unsigned b = fromInteger (a * b)
  Unsigned a - Unsigned b = fromInteger (a - b)
  negate (Unsigned a) = fromInteger (negate a)
  abs u = u
  signum (Unsigned 0) = 0
  signum _ = 1
  fromInteger i =
    let u = Unsigned i'
        Unsigned maxB = maxBound `asTypeOf` u
        i'
          | i < 0 = 0
          | i > maxB = maxB
          | otherwise = i
     in u

instance (KnownNat n) => Real (Unsigned n) where
  toRational (Unsigned i) = toRational i

instance (KnownNat n) => Enum (Unsigned n) where
  toEnum = fromInteger . toInteger
  fromEnum (Unsigned i) = fromInteger i

instance (KnownNat n) => Integral (Unsigned n) where
  quot (Unsigned a) (Unsigned b) = Unsigned (quot a b)
  rem (Unsigned a) (Unsigned b) = Unsigned (rem a b)
  div (Unsigned a) (Unsigned b) = Unsigned (div a b)
  mod (Unsigned a) (Unsigned b) = Unsigned (mod a b)
  quotRem (Unsigned a) (Unsigned b) = let (q, r) = quotRem a b in (Unsigned q, Unsigned r)
  divMod (Unsigned a) (Unsigned b) = let (q, r) = divMod a b in (Unsigned q, Unsigned r)
  toInteger (Unsigned i) = i

instance (KnownNat n) => Bits (Unsigned n) where
  Unsigned a .&. Unsigned b = fromInteger (a .&. b)
  Unsigned a .|. Unsigned b = fromInteger (a .|. b)
  Unsigned a `xor` Unsigned b = fromInteger (a `xor` b)
  complement (Unsigned a) = fromInteger (complement a)
  shift (Unsigned a) i = fromInteger (shift a i)
  rotate = error "rotate not implemented for Unsigned"
  bitSize _ = fromInteger (natVal (Proxy @n))
  bitSizeMaybe _ = Just (fromInteger (natVal (Proxy @n)))
  isSigned _ = False
  testBit (Unsigned a) i = testBit a i
  bit i = fromInteger (bit i)
  popCount (Unsigned a) = popCount a

instance (KnownNat n) => FiniteBits (Unsigned n) where
  finiteBitSize _ = fromInteger (natVal (Proxy @n))

type PositiveUnsignedError lit n typ maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'ShowType lit
        ':<>: 'Text " is (potentially) out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: add a constraint: "
          ':<>: 'ShowType (CLog 2 (lit + 1))
          ':<>: 'Text " <= "
          ':<>: 'ShowType n
          ':<>: 'Text "."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

instance
  ( Assert
      (If (lit <=? 0) (lit <=? 0) (CLog 2 (lit + 1) <=? n))
      (PositiveUnsignedError lit n (Unsigned n) ((2 ^ n) - 1))
  ) =>
  CheckedPositiveIntegerLiteral lit (Unsigned n)

instance
  (NegativeUnsignedError lit (Unsigned n) ((2 ^ n) - 1)) =>
  CheckedNegativeIntegerLiteral lit (Unsigned n)
