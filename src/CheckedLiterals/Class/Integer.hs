{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | Type classes and helper functions for checked integer literals.
module CheckedLiterals.Class.Integer where

import CheckedLiterals.Class.TemplateHaskell (maxBoundAsNat, minBoundAsNat)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeError (Assert, ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeNats (Nat, type (<=?))
import Numeric.Natural (Natural)

-- | Constraint used by the plugin to validate positive integer literals.
class CheckedPositiveIntegerLiteral (lit :: Nat) (a :: Type)

instance CheckedPositiveIntegerLiteral lit Natural

type PositiveUnsignedError lit typ maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

#define CHECKED_POSITIVE_UNSIGNED_INTEGER_INSTANCE(T) \
instance \
  (Assert (lit <=? $(maxBoundAsNat @T)) (PositiveUnsignedError lit T $(maxBoundAsNat @T))) => \
  CheckedPositiveIntegerLiteral lit T

CHECKED_POSITIVE_UNSIGNED_INTEGER_INSTANCE (Word)
CHECKED_POSITIVE_UNSIGNED_INTEGER_INSTANCE (Word8)
CHECKED_POSITIVE_UNSIGNED_INTEGER_INSTANCE (Word16)
CHECKED_POSITIVE_UNSIGNED_INTEGER_INSTANCE (Word32)
CHECKED_POSITIVE_UNSIGNED_INTEGER_INSTANCE (Word64)

instance CheckedPositiveIntegerLiteral lit Integer

type PositiveSignedError lit typ minVal maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [-"
          ':<>: 'ShowType minVal
          ':<>: 'Text " .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

#define CHECKED_POSITIVE_SIGNED_INTEGER_INSTANCE(T) \
instance \
  ( Assert \
      (lit <=? $(maxBoundAsNat @T)) \
      (PositiveSignedError lit T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  ) => \
  CheckedPositiveIntegerLiteral lit T

CHECKED_POSITIVE_SIGNED_INTEGER_INSTANCE (Int)
CHECKED_POSITIVE_SIGNED_INTEGER_INSTANCE (Int8)
CHECKED_POSITIVE_SIGNED_INTEGER_INSTANCE (Int16)
CHECKED_POSITIVE_SIGNED_INTEGER_INSTANCE (Int32)
CHECKED_POSITIVE_SIGNED_INTEGER_INSTANCE (Int64)

-- Float/Double always round (or clamp to infinity)
instance CheckedPositiveIntegerLiteral lit Float
instance CheckedPositiveIntegerLiteral lit Double

{- | Fallback instance for when the target type is not (fully) known.
Uses the @ifcxt@ mechanism: an @OVERLAPPABLE@ catch-all that produces a
helpful 'TypeError' instead of GHC's default "Could not deduce …" message.
-}
instance
  {-# INCOHERENT #-}
  ( TypeError
      ( 'Text "Cannot check literal "
          ':<>: 'ShowType lit
          ':<>: 'Text " at compile time."
          ':$$: 'Text "The target type "
            ':<>: 'ShowType a
            ':<>: 'Text " is not (fully) known."
          ':$$: 'Text "Possible fix: use a concrete type or add a 'CheckedPositiveIntegerLiteral' constraint."
          ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
      )
  ) =>
  CheckedPositiveIntegerLiteral lit a

-- | Identity helper that attaches a positive integer literal check.
checkedPositiveIntegerLiteral :: (CheckedPositiveIntegerLiteral lit a) => a -> a
checkedPositiveIntegerLiteral = id

-- | Constraint used by the plugin to validate negative integer literals.
class CheckedNegativeIntegerLiteral (lit :: Nat) (a :: Type)

type NegativeNaturalError lit typ =
  TypeError
    ( 'Text "Literal -"
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. ∞]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

instance (NegativeNaturalError lit Natural) => CheckedNegativeIntegerLiteral lit Natural

#define CHECKED_NEGATIVE_UNSIGNED_INTEGER_INSTANCE(T) \
instance (NegativeUnsignedError lit T $(maxBoundAsNat @T)) => CheckedNegativeIntegerLiteral lit T

type NegativeUnsignedError lit typ maxVal =
  TypeError
    ( 'Text "Literal -"
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

CHECKED_NEGATIVE_UNSIGNED_INTEGER_INSTANCE (Word)
CHECKED_NEGATIVE_UNSIGNED_INTEGER_INSTANCE (Word8)
CHECKED_NEGATIVE_UNSIGNED_INTEGER_INSTANCE (Word16)
CHECKED_NEGATIVE_UNSIGNED_INTEGER_INSTANCE (Word32)
CHECKED_NEGATIVE_UNSIGNED_INTEGER_INSTANCE (Word64)

instance CheckedNegativeIntegerLiteral lit Integer

type NegativeSignedError lit typ minVal maxVal =
  TypeError
    ( 'Text "Literal -"
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [-"
          ':<>: 'ShowType minVal
          ':<>: 'Text " .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

#define CHECKED_NEGATIVE_SIGNED_INTEGER_INSTANCE(T) \
instance \
  ( Assert \
      (lit <=? $(minBoundAsNat @T)) \
      (NegativeSignedError lit T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  ) => \
  CheckedNegativeIntegerLiteral lit T

CHECKED_NEGATIVE_SIGNED_INTEGER_INSTANCE (Int)
CHECKED_NEGATIVE_SIGNED_INTEGER_INSTANCE (Int8)
CHECKED_NEGATIVE_SIGNED_INTEGER_INSTANCE (Int16)
CHECKED_NEGATIVE_SIGNED_INTEGER_INSTANCE (Int32)
CHECKED_NEGATIVE_SIGNED_INTEGER_INSTANCE (Int64)

-- Float/Double always round (or clamp to infinity)
instance CheckedNegativeIntegerLiteral lit Float
instance CheckedNegativeIntegerLiteral lit Double

-- | Fallback instance for when the target type is not (fully) known.
instance
  {-# INCOHERENT #-}
  ( TypeError
      ( 'Text "Cannot check literal -"
          ':<>: 'ShowType lit
          ':<>: 'Text " at compile time."
          ':$$: 'Text "The target type "
            ':<>: 'ShowType a
            ':<>: 'Text " is not (fully) known."
          ':$$: 'Text "Possible fix: use a concrete type or add a 'CheckedNegativeIntegerLiteral' constraint."
          ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
      )
  ) =>
  CheckedNegativeIntegerLiteral lit a

-- | Identity helper that attaches a negative integer literal check.
checkedNegativeIntegerLiteral :: (CheckedNegativeIntegerLiteral lit a) => a -> a
checkedNegativeIntegerLiteral = id
