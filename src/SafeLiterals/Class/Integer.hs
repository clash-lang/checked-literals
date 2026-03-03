{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module SafeLiterals.Class.Integer where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeError (Assert, ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeNats (Nat, type (<=?))
import Numeric.Natural (Natural)
import SafeLiterals.Class.TemplateHaskell (maxBoundAsNat, minBoundAsNat)

class SafePositiveIntegerLiteral (lit :: Nat) (a :: Type)

instance SafePositiveIntegerLiteral lit Natural

type PositiveUnsignedError lit typ maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

#define SAFE_POSITIVE_UNSIGNED_INTEGER_INSTANCE(T) \
instance \
  (Assert (lit <=? $(maxBoundAsNat @T)) (PositiveUnsignedError lit T $(maxBoundAsNat @T))) => \
  SafePositiveIntegerLiteral lit T

SAFE_POSITIVE_UNSIGNED_INTEGER_INSTANCE (Word)
SAFE_POSITIVE_UNSIGNED_INTEGER_INSTANCE (Word8)
SAFE_POSITIVE_UNSIGNED_INTEGER_INSTANCE (Word16)
SAFE_POSITIVE_UNSIGNED_INTEGER_INSTANCE (Word32)
SAFE_POSITIVE_UNSIGNED_INTEGER_INSTANCE (Word64)

instance SafePositiveIntegerLiteral lit Integer

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
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

#define SAFE_POSITIVE_SIGNED_INTEGER_INSTANCE(T) \
instance \
  ( Assert \
      (lit <=? $(maxBoundAsNat @T)) \
      (PositiveSignedError lit T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  ) => \
  SafePositiveIntegerLiteral lit T

SAFE_POSITIVE_SIGNED_INTEGER_INSTANCE (Int)
SAFE_POSITIVE_SIGNED_INTEGER_INSTANCE (Int8)
SAFE_POSITIVE_SIGNED_INTEGER_INSTANCE (Int16)
SAFE_POSITIVE_SIGNED_INTEGER_INSTANCE (Int32)
SAFE_POSITIVE_SIGNED_INTEGER_INSTANCE (Int64)

-- Float/Double always round (or clamp to infinity)
instance SafePositiveIntegerLiteral lit Float
instance SafePositiveIntegerLiteral lit Double

safePositiveIntegerLiteral :: (SafePositiveIntegerLiteral lit a) => a -> a
safePositiveIntegerLiteral = id

class SafeNegativeIntegerLiteral (lit :: Nat) (a :: Type)

type NegativeNaturalError lit typ =
  TypeError
    ( 'Text "Literal -"
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. ∞]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

instance (NegativeNaturalError lit Natural) => SafeNegativeIntegerLiteral lit Natural

#define SAFE_NEGATIVE_UNSIGNED_INTEGER_INSTANCE(T) \
instance (NegativeUnsignedError lit T $(maxBoundAsNat @T)) => SafeNegativeIntegerLiteral lit T

type NegativeUnsignedError lit typ maxVal =
  TypeError
    ( 'Text "Literal -"
        ':<>: 'ShowType lit
        ':<>: 'Text " is out of bounds."
        ':$$: 'ShowType typ
          ':<>: 'Text " has bounds: [0 .. "
          ':<>: 'ShowType maxVal
          ':<>: 'Text "]."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

SAFE_NEGATIVE_UNSIGNED_INTEGER_INSTANCE (Word)
SAFE_NEGATIVE_UNSIGNED_INTEGER_INSTANCE (Word8)
SAFE_NEGATIVE_UNSIGNED_INTEGER_INSTANCE (Word16)
SAFE_NEGATIVE_UNSIGNED_INTEGER_INSTANCE (Word32)
SAFE_NEGATIVE_UNSIGNED_INTEGER_INSTANCE (Word64)

instance SafeNegativeIntegerLiteral lit Integer

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
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
    )

#define SAFE_NEGATIVE_SIGNED_INTEGER_INSTANCE(T) \
instance \
  ( Assert \
      (lit <=? $(minBoundAsNat @T)) \
      (NegativeSignedError lit T $(minBoundAsNat @T) $(maxBoundAsNat @T)) \
  ) => \
  SafeNegativeIntegerLiteral lit T

SAFE_NEGATIVE_SIGNED_INTEGER_INSTANCE (Int)
SAFE_NEGATIVE_SIGNED_INTEGER_INSTANCE (Int8)
SAFE_NEGATIVE_SIGNED_INTEGER_INSTANCE (Int16)
SAFE_NEGATIVE_SIGNED_INTEGER_INSTANCE (Int32)
SAFE_NEGATIVE_SIGNED_INTEGER_INSTANCE (Int64)

-- Float/Double always round (or clamp to infinity)
instance SafeNegativeIntegerLiteral lit Float
instance SafeNegativeIntegerLiteral lit Double

safeNegativeIntegerLiteral :: (SafeNegativeIntegerLiteral lit a) => a -> a
safeNegativeIntegerLiteral = id
