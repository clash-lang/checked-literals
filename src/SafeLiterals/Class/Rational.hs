{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module SafeLiterals.Class.Rational where

import Data.Fixed (E0, E1, E2, E3, E6, E9, Fixed)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeError (Assert, ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeLits (Nat, Symbol, type Mod, type (<=?))
import GHC.TypeNats (type (*))
import Numeric.Natural (Natural)
import SafeLiterals.Class.TemplateHaskell (maxBoundAsNat, minBoundAsNat)

class
  SafePositiveRationalLiteral
    (literalAsString :: Symbol)
    (numerator :: Nat)
    (denominator :: Nat)
    (a :: Type)

instance SafePositiveRationalLiteral str num den Rational
instance SafePositiveRationalLiteral str num den (Ratio Natural)
instance SafePositiveRationalLiteral str num den Float
instance SafePositiveRationalLiteral str num den Double

type FixedRoundingError str fixedType resolution =
  TypeError
    ( 'Text "Literal "
        ':<>: 'ShowType str
        ':<>: 'Text " requires rounding for "
        ':<>: 'ShowType fixedType
        ':<>: 'Text " (resolution 1/"
        ':<>: 'ShowType resolution
        ':<>: 'Text ")."
        ':$$: 'Text "The literal cannot be represented exactly without rounding."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

#define SAFE_POSITIVE_FIXED_RATIONAL_INSTANCE(E, RES) \
instance \
  ( Assert \
      ((Mod (num * RES) den) <=? 0) \
      (FixedRoundingError str (Fixed E) RES) \
  ) => \
  SafePositiveRationalLiteral str num den (Fixed E)

SAFE_POSITIVE_FIXED_RATIONAL_INSTANCE (E0, 1)
SAFE_POSITIVE_FIXED_RATIONAL_INSTANCE (E1, 10)
SAFE_POSITIVE_FIXED_RATIONAL_INSTANCE (E2, 100)
SAFE_POSITIVE_FIXED_RATIONAL_INSTANCE (E3, 1_000)
SAFE_POSITIVE_FIXED_RATIONAL_INSTANCE (E6, 1_000_000)
SAFE_POSITIVE_FIXED_RATIONAL_INSTANCE (E9, 1_000_000_000)
SAFE_POSITIVE_FIXED_RATIONAL_INSTANCE ((res :: Nat), res)

safePositiveRationalLiteral :: (SafePositiveRationalLiteral fixed num den a) => a -> a
safePositiveRationalLiteral = id

class SafeNegativeRationalLiteral (str :: Symbol) (numerator :: Nat) (denominator :: Nat) (a :: Type)

instance SafeNegativeRationalLiteral str num den Rational
instance SafeNegativeRationalLiteral str num den Float
instance SafeNegativeRationalLiteral str num den Double

#define SAFE_NEGATIVE_FIXED_RATIONAL_INSTANCE(E, RES) \
instance \
  ( Assert \
      ((Mod (num * RES) den) <=? 0) \
      (FixedRoundingError str (Fixed E) RES) \
  ) => \
  SafeNegativeRationalLiteral str num den (Fixed E)

SAFE_NEGATIVE_FIXED_RATIONAL_INSTANCE (E0, 1)
SAFE_NEGATIVE_FIXED_RATIONAL_INSTANCE (E1, 10)
SAFE_NEGATIVE_FIXED_RATIONAL_INSTANCE (E2, 100)
SAFE_NEGATIVE_FIXED_RATIONAL_INSTANCE (E3, 1_000)
SAFE_NEGATIVE_FIXED_RATIONAL_INSTANCE (E6, 1_000_000)
SAFE_NEGATIVE_FIXED_RATIONAL_INSTANCE (E9, 1_000_000_000)
SAFE_NEGATIVE_FIXED_RATIONAL_INSTANCE ((res :: Nat), res)

safeNegativeRationalLiteral :: (SafeNegativeRationalLiteral fixed num den a) => a -> a
safeNegativeRationalLiteral = id

type PositiveUnsignedRatioNotRepresentable str num den typ maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'Text str
        ':<>: 'Text " ("
        ':<>: 'ShowType num
        ':<>: 'Text " % "
        ':<>: 'ShowType den
        ':<>: 'Text ")"
        ':<>: 'Text " cannot by represented by "
        ':<>: 'ShowType typ
        ':<>: 'Text "."
        ':$$: 'Text "Make sure both the numerator and the denominator fit "
          ':<>: 'ShowType typ
          ':<>: 'Text "."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

#define SAFE_POSITIVE_UNSIGNED_RATIONAL_INSTANCE(T) \
instance \
  ( Assert \
      (num <=? $(maxBoundAsNat @T)) \
      (PositiveUnsignedRatioNotRepresentable str num den T $(maxBoundAsNat @T)) \
  \
  , Assert \
      (den <=? $(maxBoundAsNat @T)) \
      (PositiveUnsignedRatioNotRepresentable str num den T $(maxBoundAsNat @T)) \
  ) => \
  SafePositiveRationalLiteral str num den (Ratio T)

SAFE_POSITIVE_UNSIGNED_RATIONAL_INSTANCE (Word)
SAFE_POSITIVE_UNSIGNED_RATIONAL_INSTANCE (Word64)
SAFE_POSITIVE_UNSIGNED_RATIONAL_INSTANCE (Word32)
SAFE_POSITIVE_UNSIGNED_RATIONAL_INSTANCE (Word16)
SAFE_POSITIVE_UNSIGNED_RATIONAL_INSTANCE (Word8)

type PositiveSignedRatioNotRepresentable str num den typ minVal maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'Text str
        ':<>: 'Text " ("
        ':<>: 'ShowType num
        ':<>: 'Text " % "
        ':<>: 'ShowType den
        ':<>: 'Text ")"
        ':<>: 'Text " cannot by represented by "
        ':<>: 'ShowType typ
        ':<>: 'Text "."
        ':$$: 'Text "Make sure both the numerator and the denominator fit "
          ':<>: 'ShowType typ
          ':<>: 'Text "."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [-"
          ':<>: 'ShowType minVal
          ':<>: 'Text " .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

#define SAFE_POSITIVE_SIGNED_RATIONAL_INSTANCE(T) \
instance \
  ( Assert \
      (num <=? $(maxBoundAsNat @T)) \
      (PositiveSignedRatioNotRepresentable str num den T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  \
  , Assert \
      (den <=? $(maxBoundAsNat @T)) \
      (PositiveSignedRatioNotRepresentable str num den T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  ) => \
  SafePositiveRationalLiteral str num den (Ratio T)

SAFE_POSITIVE_SIGNED_RATIONAL_INSTANCE (Int)
SAFE_POSITIVE_SIGNED_RATIONAL_INSTANCE (Int64)
SAFE_POSITIVE_SIGNED_RATIONAL_INSTANCE (Int32)
SAFE_POSITIVE_SIGNED_RATIONAL_INSTANCE (Int16)
SAFE_POSITIVE_SIGNED_RATIONAL_INSTANCE (Int8)

type NegativeNaturalRatioNotRepresentable str num den typ =
  TypeError
    ( 'Text "Literal "
        ':<>: 'Text str
        ':<>: 'Text " (-"
        ':<>: 'ShowType num
        ':<>: 'Text " % "
        ':<>: 'ShowType den
        ':<>: 'Text ")"
        ':<>: 'Text " cannot by represented by "
        ':<>: 'ShowType (Ratio typ)
        ':<>: 'Text "."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. ∞]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

instance
  (NegativeNaturalRatioNotRepresentable str num den Natural) =>
  SafeNegativeRationalLiteral str num den (Ratio Natural)

type NegativeUnsignedRatioNotRepresentable str num den typ maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'Text str
        ':<>: 'Text " cannot by represented by "
        ':<>: 'ShowType (Ratio typ)
        ':<>: 'Text "."
        ':$$: 'ShowType typ
          ':<>: 'Text " cannot represent negative numbers."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

#define SAFE_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE(T) \
instance \
  (NegativeUnsignedRatioNotRepresentable str num den T $(maxBoundAsNat @T)) => \
  SafeNegativeRationalLiteral str num den (Ratio T)

SAFE_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE (Word)
SAFE_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE (Word64)
SAFE_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE (Word32)
SAFE_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE (Word16)
SAFE_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE (Word8)

type NegativeSignedRatioNotRepresentable str num den typ minVal maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'Text str
        ':<>: 'Text " (-"
        ':<>: 'ShowType num
        ':<>: 'Text " % "
        ':<>: 'ShowType den
        ':<>: 'Text ")"
        ':<>: 'Text " cannot by represented by "
        ':<>: 'ShowType typ
        ':<>: 'Text "."
        ':$$: 'Text "Make sure both the numerator and the denominator fit "
          ':<>: 'ShowType typ
          ':<>: 'Text "."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [-"
          ':<>: 'ShowType minVal
          ':<>: 'Text " .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

#define SAFE_NEGATIVE_SIGNED_RATIONAL_INSTANCE(T) \
instance \
  ( Assert \
      (num <=? $(minBoundAsNat @T)) \
      (NegativeSignedRatioNotRepresentable str num den T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  \
  , Assert \
      (den <=? $(maxBoundAsNat @T)) \
      (NegativeSignedRatioNotRepresentable str num den T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  ) => \
  SafeNegativeRationalLiteral str num den (Ratio T)

SAFE_NEGATIVE_SIGNED_RATIONAL_INSTANCE (Int)
SAFE_NEGATIVE_SIGNED_RATIONAL_INSTANCE (Int64)
SAFE_NEGATIVE_SIGNED_RATIONAL_INSTANCE (Int32)
SAFE_NEGATIVE_SIGNED_RATIONAL_INSTANCE (Int16)
SAFE_NEGATIVE_SIGNED_RATIONAL_INSTANCE (Int8)
