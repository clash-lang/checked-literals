{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | Type classes and helper functions for checked rational literals.
module CheckedLiterals.Class.Rational where

import CheckedLiterals.Class.TemplateHaskell (maxBoundAsNat, minBoundAsNat)
import Data.Fixed (E0, E1, E2, E3, E6, E9, Fixed)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeError (Assert, ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeLits (Nat, Symbol, type Mod, type (<=?))
import GHC.TypeNats (type (*))
import Numeric.Natural (Natural)

-- | Constraint used by the plugin to validate positive rational literals.
class
  CheckedPositiveRationalLiteral
    (literalAsString :: Symbol)
    (numerator :: Nat)
    (denominator :: Nat)
    (a :: Type)

instance CheckedPositiveRationalLiteral str num den Rational
instance CheckedPositiveRationalLiteral str num den (Ratio Natural)
instance CheckedPositiveRationalLiteral str num den Float
instance CheckedPositiveRationalLiteral str num den Double

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
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

#define CHECKED_POSITIVE_FIXED_RATIONAL_INSTANCE(E, RES) \
instance \
  ( Assert \
      ((Mod (num * RES) den) <=? 0) \
      (FixedRoundingError str (Fixed E) RES) \
  ) => \
  CheckedPositiveRationalLiteral str num den (Fixed E)

CHECKED_POSITIVE_FIXED_RATIONAL_INSTANCE (E0, 1)
CHECKED_POSITIVE_FIXED_RATIONAL_INSTANCE (E1, 10)
CHECKED_POSITIVE_FIXED_RATIONAL_INSTANCE (E2, 100)
CHECKED_POSITIVE_FIXED_RATIONAL_INSTANCE (E3, 1_000)
CHECKED_POSITIVE_FIXED_RATIONAL_INSTANCE (E6, 1_000_000)
CHECKED_POSITIVE_FIXED_RATIONAL_INSTANCE (E9, 1_000_000_000)
CHECKED_POSITIVE_FIXED_RATIONAL_INSTANCE ((res :: Nat), res)

-- | Fallback instance for when the target type is not (fully) known.
instance
  {-# INCOHERENT #-}
  ( TypeError
      ( 'Text "Cannot check literal "
          ':<>: 'ShowType str
          ':<>: 'Text " at compile time."
          ':$$: 'Text "The target type "
            ':<>: 'ShowType a
            ':<>: 'Text " is not (fully) known."
          ':$$: 'Text "Possible fix: use a concrete type or add a 'CheckedPositiveRationalLiteral' constraint."
          ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
      )
  ) =>
  CheckedPositiveRationalLiteral str num den a

-- | Identity helper that attaches a positive rational literal check.
checkedPositiveRationalLiteral :: (CheckedPositiveRationalLiteral fixed num den a) => a -> a
checkedPositiveRationalLiteral = id

-- | Constraint used by the plugin to validate negative rational literals.
class CheckedNegativeRationalLiteral (str :: Symbol) (numerator :: Nat) (denominator :: Nat) (a :: Type)

instance CheckedNegativeRationalLiteral str num den Rational
instance CheckedNegativeRationalLiteral str num den Float
instance CheckedNegativeRationalLiteral str num den Double

#define CHECKED_NEGATIVE_FIXED_RATIONAL_INSTANCE(E, RES) \
instance \
  ( Assert \
      ((Mod (num * RES) den) <=? 0) \
      (FixedRoundingError str (Fixed E) RES) \
  ) => \
  CheckedNegativeRationalLiteral str num den (Fixed E)

CHECKED_NEGATIVE_FIXED_RATIONAL_INSTANCE (E0, 1)
CHECKED_NEGATIVE_FIXED_RATIONAL_INSTANCE (E1, 10)
CHECKED_NEGATIVE_FIXED_RATIONAL_INSTANCE (E2, 100)
CHECKED_NEGATIVE_FIXED_RATIONAL_INSTANCE (E3, 1_000)
CHECKED_NEGATIVE_FIXED_RATIONAL_INSTANCE (E6, 1_000_000)
CHECKED_NEGATIVE_FIXED_RATIONAL_INSTANCE (E9, 1_000_000_000)
CHECKED_NEGATIVE_FIXED_RATIONAL_INSTANCE ((res :: Nat), res)

-- | Fallback instance for when the target type is not (fully) known.
instance
  {-# INCOHERENT #-}
  ( TypeError
      ( 'Text "Cannot check literal "
          ':<>: 'ShowType str
          ':<>: 'Text " at compile time."
          ':$$: 'Text "The target type "
            ':<>: 'ShowType a
            ':<>: 'Text " is not (fully) known."
          ':$$: 'Text "Possible fix: use a concrete type or add a 'CheckedNegativeRationalLiteral' constraint."
          ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
      )
  ) =>
  CheckedNegativeRationalLiteral str num den a

-- | Identity helper that attaches a negative rational literal check.
checkedNegativeRationalLiteral :: (CheckedNegativeRationalLiteral fixed num den a) => a -> a
checkedNegativeRationalLiteral = id

type PositiveUnsignedRatioNotRepresentable str num den typ maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'Text str
        ':<>: 'Text " ("
        ':<>: 'ShowType num
        ':<>: 'Text " % "
        ':<>: 'ShowType den
        ':<>: 'Text ")"
        ':<>: 'Text " cannot be represented by "
        ':<>: 'ShowType typ
        ':<>: 'Text "."
        ':$$: 'Text "Make sure both the numerator and the denominator fit "
          ':<>: 'ShowType typ
          ':<>: 'Text "."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

#define CHECKED_POSITIVE_UNSIGNED_RATIONAL_INSTANCE(T) \
instance \
  ( Assert \
      (num <=? $(maxBoundAsNat @T)) \
      (PositiveUnsignedRatioNotRepresentable str num den T $(maxBoundAsNat @T)) \
  \
  , Assert \
      (den <=? $(maxBoundAsNat @T)) \
      (PositiveUnsignedRatioNotRepresentable str num den T $(maxBoundAsNat @T)) \
  ) => \
  CheckedPositiveRationalLiteral str num den (Ratio T)

CHECKED_POSITIVE_UNSIGNED_RATIONAL_INSTANCE (Word)
CHECKED_POSITIVE_UNSIGNED_RATIONAL_INSTANCE (Word64)
CHECKED_POSITIVE_UNSIGNED_RATIONAL_INSTANCE (Word32)
CHECKED_POSITIVE_UNSIGNED_RATIONAL_INSTANCE (Word16)
CHECKED_POSITIVE_UNSIGNED_RATIONAL_INSTANCE (Word8)

type PositiveSignedRatioNotRepresentable str num den typ minVal maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'Text str
        ':<>: 'Text " ("
        ':<>: 'ShowType num
        ':<>: 'Text " % "
        ':<>: 'ShowType den
        ':<>: 'Text ")"
        ':<>: 'Text " cannot be represented by "
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
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

#define CHECKED_POSITIVE_SIGNED_RATIONAL_INSTANCE(T) \
instance \
  ( Assert \
      (num <=? $(maxBoundAsNat @T)) \
      (PositiveSignedRatioNotRepresentable str num den T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  \
  , Assert \
      (den <=? $(maxBoundAsNat @T)) \
      (PositiveSignedRatioNotRepresentable str num den T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  ) => \
  CheckedPositiveRationalLiteral str num den (Ratio T)

CHECKED_POSITIVE_SIGNED_RATIONAL_INSTANCE (Int)
CHECKED_POSITIVE_SIGNED_RATIONAL_INSTANCE (Int64)
CHECKED_POSITIVE_SIGNED_RATIONAL_INSTANCE (Int32)
CHECKED_POSITIVE_SIGNED_RATIONAL_INSTANCE (Int16)
CHECKED_POSITIVE_SIGNED_RATIONAL_INSTANCE (Int8)

type NegativeNaturalRatioNotRepresentable str num den typ =
  TypeError
    ( 'Text "Literal "
        ':<>: 'Text str
        ':<>: 'Text " (-"
        ':<>: 'ShowType num
        ':<>: 'Text " % "
        ':<>: 'ShowType den
        ':<>: 'Text ")"
        ':<>: 'Text " cannot be represented by "
        ':<>: 'ShowType (Ratio typ)
        ':<>: 'Text "."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. ∞]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

instance
  (NegativeNaturalRatioNotRepresentable str num den Natural) =>
  CheckedNegativeRationalLiteral str num den (Ratio Natural)

type NegativeUnsignedRatioNotRepresentable str num den typ maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'Text str
        ':<>: 'Text " cannot be represented by "
        ':<>: 'ShowType (Ratio typ)
        ':<>: 'Text "."
        ':$$: 'ShowType typ
          ':<>: 'Text " cannot represent negative numbers."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

#define CHECKED_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE(T) \
instance \
  (NegativeUnsignedRatioNotRepresentable str num den T $(maxBoundAsNat @T)) => \
  CheckedNegativeRationalLiteral str num den (Ratio T)

CHECKED_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE (Word)
CHECKED_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE (Word64)
CHECKED_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE (Word32)
CHECKED_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE (Word16)
CHECKED_NEGATIVE_UNSIGNED_RATIONAL_INSTANCE (Word8)

type NegativeSignedRatioNotRepresentable str num den typ minVal maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'Text str
        ':<>: 'Text " (-"
        ':<>: 'ShowType num
        ':<>: 'Text " % "
        ':<>: 'ShowType den
        ':<>: 'Text ")"
        ':<>: 'Text " cannot be represented by "
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
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

#define CHECKED_NEGATIVE_SIGNED_RATIONAL_INSTANCE(T) \
instance \
  ( Assert \
      (num <=? $(minBoundAsNat @T)) \
      (NegativeSignedRatioNotRepresentable str num den T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  \
  , Assert \
      (den <=? $(maxBoundAsNat @T)) \
      (NegativeSignedRatioNotRepresentable str num den T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  ) => \
  CheckedNegativeRationalLiteral str num den (Ratio T)

CHECKED_NEGATIVE_SIGNED_RATIONAL_INSTANCE (Int)
CHECKED_NEGATIVE_SIGNED_RATIONAL_INSTANCE (Int64)
CHECKED_NEGATIVE_SIGNED_RATIONAL_INSTANCE (Int32)
CHECKED_NEGATIVE_SIGNED_RATIONAL_INSTANCE (Int16)
CHECKED_NEGATIVE_SIGNED_RATIONAL_INSTANCE (Int8)
