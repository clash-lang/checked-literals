{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- | Identity helper that attaches a negative integer literal check.
checkedNegativeIntegerLiteral :: (CheckedNegativeIntegerLiteral lit a) => a -> a
checkedNegativeIntegerLiteral = id
