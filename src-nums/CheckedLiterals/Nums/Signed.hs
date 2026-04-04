{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CheckedLiterals.Nums.Signed (
  Signed (..),
) where

import CheckedLiterals (
  CheckedNegativeIntegerLiteral,
  CheckedPositiveIntegerLiteral,
 )
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Proxy (Proxy (..))
import Data.Type.Bool (type If)
import GHC.TypeError (Assert, ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeLits (KnownNat, Nat, natVal, type (+), type (-), type (<=?), type (^))
import GHC.TypeLits.Extra (CLog)

-- | Signed integer with @n@ bits (including sign bit)
newtype Signed (n :: Nat) = Signed Integer
  deriving (Eq, Ord)

instance (KnownNat n) => Show (Signed n) where
  show = show . toInteger

instance (KnownNat n) => Bounded (Signed n) where
  minBound =
    let n = natVal (Proxy @n)
     in Signed $
          if n == 0
            then 0
            else negate (2 ^ (n - 1))
  maxBound =
    let n = natVal (Proxy @n)
     in Signed $
          if n == 0
            then 0
            else 2 ^ (n - 1) - 1

instance (KnownNat n) => Num (Signed n) where
  Signed a + Signed b = fromInteger (a + b)
  Signed a * Signed b = fromInteger (a * b)
  Signed a - Signed b = fromInteger (a - b)
  negate (Signed a) = fromInteger (negate a)
  abs (Signed a) = fromInteger (abs a)
  signum (Signed a) = fromInteger (signum a)
  fromInteger i =
    let s = Signed i'
        Signed minB = minBound `asTypeOf` s
        Signed maxB = maxBound `asTypeOf` s
        i'
          | i < minB = minB
          | i > maxB = maxB
          | otherwise = i
     in s

instance (KnownNat n) => Real (Signed n) where
  toRational (Signed i) = toRational i

instance (KnownNat n) => Enum (Signed n) where
  toEnum = fromInteger . toInteger
  fromEnum (Signed i) = fromInteger i

instance (KnownNat n) => Integral (Signed n) where
  quot (Signed a) (Signed b) = Signed (quot a b)
  rem (Signed a) (Signed b) = Signed (rem a b)
  div (Signed a) (Signed b) = Signed (div a b)
  mod (Signed a) (Signed b) = Signed (mod a b)
  quotRem (Signed a) (Signed b) = let (q, r) = quotRem a b in (Signed q, Signed r)
  divMod (Signed a) (Signed b) = let (q, r) = divMod a b in (Signed q, Signed r)
  toInteger (Signed i) = i

instance (KnownNat n) => Bits (Signed n) where
  Signed a .&. Signed b = fromInteger (a .&. b)
  Signed a .|. Signed b = fromInteger (a .|. b)
  Signed a `xor` Signed b = fromInteger (a `xor` b)
  complement (Signed a) = fromInteger (complement a)
  shift (Signed a) i = fromInteger (shift a i)
  rotate = error "rotate not implemented for Signed"
  bitSize _s = fromInteger (natVal (Proxy @n))
  bitSizeMaybe _s = Just (fromInteger (natVal (Proxy @n)))
  isSigned _ = True
  testBit (Signed a) i = testBit a i
  bit i = fromInteger (bit i)
  popCount (Signed a) = popCount a

instance (KnownNat n) => FiniteBits (Signed n) where
  finiteBitSize _ = fromInteger (natVal (Proxy @n))

type PositiveSignedError lit n typ minVal maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'ShowType lit
        ':<>: 'Text " is (potentially) out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [-"
          ':<>: 'ShowType minVal
          ':<>: 'Text " .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: add a constraint: "
          ':<>: 'ShowType (CLog 2 (lit + 1) + 1)
          ':<>: 'Text " <= "
          ':<>: 'ShowType n
          ':<>: 'Text "."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

instance
  ( Assert
      (If (lit <=? 0) (lit <=? 0) (CLog 2 (lit + 1) + 1 <=? n))
      (PositiveSignedError lit n (Signed n) (2 ^ (n - 1)) ((2 ^ (n - 1)) - 1))
  ) =>
  CheckedPositiveIntegerLiteral lit (Signed n)

type NegativeSignedError lit n typ minVal maxVal =
  TypeError
    ( 'Text "Literal -"
        ':<>: 'ShowType lit
        ':<>: 'Text " is (potentially) out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [-"
          ':<>: 'ShowType minVal
          ':<>: 'Text " .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: add a constraint: "
          ':<>: 'ShowType (CLog 2 lit + 1)
          ':<>: 'Text " <= "
          ':<>: 'ShowType n
          ':<>: 'Text "."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

instance
  ( Assert
      (If (lit <=? 0) (lit <=? 0) (CLog 2 lit + 1 <=? n))
      (NegativeSignedError lit n (Signed n) (2 ^ (n - 1)) ((2 ^ (n - 1)) - 1))
  ) =>
  CheckedNegativeIntegerLiteral lit (Signed n)
