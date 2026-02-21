{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module SafeLiterals.Class where

import Data.Complex (Complex)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Monoid (Ap, Product, Sum)
import Data.Ord (Down)
import Data.Ratio (Ratio)
import Data.Semigroup (Max, Min)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeError (Assert, ErrorMessage (ShowType, Text, (:<>:), (:$$:)), TypeError)
import GHC.TypeNats (Nat, type (<=?))
import Numeric.Natural (Natural)

import SafeLiterals.Class.TemplateHaskell (maxBoundAsNat, minBoundAsNat)

type PositiveIntegerTooLargeError lit typ maxVal = TypeError
  ( 'Text "Literal "
      ':<>: 'ShowType lit
      ':<>: 'Text " is out of bounds."
      ':$$: 'ShowType typ
      ':<>: 'Text " has bounds: [0 .. "
      ':<>: 'ShowType maxVal
      ':<>: 'Text "]."
      ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
  )

class SafePositiveIntegerLiteral (lit :: Nat) (a :: Type)

instance SafePositiveIntegerLiteral lit Natural
instance (Assert (lit <=? $(maxBoundAsNat @Word)) (PositiveIntegerTooLargeError lit Word $(maxBoundAsNat @Word))) => SafePositiveIntegerLiteral lit Word
instance (Assert (lit <=? $(maxBoundAsNat @Word8)) (PositiveIntegerTooLargeError lit Word8 $(maxBoundAsNat @Word8))) => SafePositiveIntegerLiteral lit Word8
instance (Assert (lit <=? $(maxBoundAsNat @Word16)) (PositiveIntegerTooLargeError lit Word16 $(maxBoundAsNat @Word16))) => SafePositiveIntegerLiteral lit Word16
instance (Assert (lit <=? $(maxBoundAsNat @Word32)) (PositiveIntegerTooLargeError lit Word32 $(maxBoundAsNat @Word32))) => SafePositiveIntegerLiteral lit Word32
instance (Assert (lit <=? $(maxBoundAsNat @Word64)) (PositiveIntegerTooLargeError lit Word64 $(maxBoundAsNat @Word64))) => SafePositiveIntegerLiteral lit Word64

instance SafePositiveIntegerLiteral lit Integer
instance (Assert (lit <=? $(maxBoundAsNat @Int)) (PositiveIntegerTooLargeError lit Int $(maxBoundAsNat @Int))) => SafePositiveIntegerLiteral lit Int
instance (Assert (lit <=? $(maxBoundAsNat @Int8)) (PositiveIntegerTooLargeError lit Int8 $(maxBoundAsNat @Int8))) => SafePositiveIntegerLiteral lit Int8
instance (Assert (lit <=? $(maxBoundAsNat @Int16)) (PositiveIntegerTooLargeError lit Int16 $(maxBoundAsNat @Int16))) => SafePositiveIntegerLiteral lit Int16
instance (Assert (lit <=? $(maxBoundAsNat @Int32)) (PositiveIntegerTooLargeError lit Int32 $(maxBoundAsNat @Int32))) => SafePositiveIntegerLiteral lit Int32
instance (Assert (lit <=? $(maxBoundAsNat @Int64)) (PositiveIntegerTooLargeError lit Int64 $(maxBoundAsNat @Int64))) => SafePositiveIntegerLiteral lit Int64

-- Not clear what to do with edge cases:
instance TypeError ('Text "Float unsupported by `safe-literals`." ':$$: 'Text "Potential fix: use rational notation, e.g. " ':<>: ShowType lit ':<>: 'Text ".0.") => SafePositiveIntegerLiteral lit Float
instance TypeError ('Text "Double unsupported by `safe-literals`." ':$$: 'Text "Potential fix: use rational notation, e.g. " ':<>: ShowType lit ':<>: 'Text ".0.") => SafePositiveIntegerLiteral lit Double

instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Complex a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Ratio a)

instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Max a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Min a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Identity a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Down a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Product a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Sum a)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Const a b)
instance (SafePositiveIntegerLiteral lit a) => SafePositiveIntegerLiteral lit (Ap f a)

safePositiveIntegerLiteral :: (SafePositiveIntegerLiteral lit a) => a -> a
safePositiveIntegerLiteral = id

type NegativeIntegerTypeError lit typ maxVal = TypeError
  ( 'Text "Negative literal -"
      ':<>: 'ShowType lit
      ':<>: 'Text " is out of bounds."
      ':$$: 'ShowType typ
      ':<>: 'Text " has bounds: [0 .. "
      ':<>: 'ShowType maxVal
      ':<>: 'Text "]."
      ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
  )

type NegativeIntegerUnboundedTypeError lit typ = TypeError
  ( 'Text "Negative literal -"
      ':<>: 'ShowType lit
      ':<>: 'Text " is out of bounds."
      ':$$: 'ShowType typ
      ':<>: 'Text " has bounds: [0 .. ∞]."
      ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'SafeLiterals' to bypass this check."
  )

type NegativeIntegerTooLargeError lit typ minVal maxVal = TypeError
  ( 'Text "Negative literal -"
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

class SafeNegativeIntegerLiteral (lit :: Nat) (a :: Type)

instance (NegativeIntegerUnboundedTypeError lit Natural) => SafeNegativeIntegerLiteral lit Natural
instance (NegativeIntegerTypeError lit Word $(maxBoundAsNat @Word)) => SafeNegativeIntegerLiteral lit Word
instance (NegativeIntegerTypeError lit Word8 $(maxBoundAsNat @Word8)) => SafeNegativeIntegerLiteral lit Word8
instance (NegativeIntegerTypeError lit Word16 $(maxBoundAsNat @Word16)) => SafeNegativeIntegerLiteral lit Word16
instance (NegativeIntegerTypeError lit Word32 $(maxBoundAsNat @Word32)) => SafeNegativeIntegerLiteral lit Word32
instance (NegativeIntegerTypeError lit Word64 $(maxBoundAsNat @Word64)) => SafeNegativeIntegerLiteral lit Word64

instance SafeNegativeIntegerLiteral lit Integer
instance (Assert (lit <=? $(minBoundAsNat @Int)) (NegativeIntegerTooLargeError lit Int $(minBoundAsNat @Int) $(maxBoundAsNat @Int))) => SafeNegativeIntegerLiteral lit Int
instance (Assert (lit <=? $(minBoundAsNat @Int8)) (NegativeIntegerTooLargeError lit Int8 $(minBoundAsNat @Int8) $(maxBoundAsNat @Int8))) => SafeNegativeIntegerLiteral lit Int8
instance (Assert (lit <=? $(minBoundAsNat @Int16)) (NegativeIntegerTooLargeError lit Int16 $(minBoundAsNat @Int16) $(maxBoundAsNat @Int16))) => SafeNegativeIntegerLiteral lit Int16
instance (Assert (lit <=? $(minBoundAsNat @Int32)) (NegativeIntegerTooLargeError lit Int32 $(minBoundAsNat @Int32) $(maxBoundAsNat @Int32))) => SafeNegativeIntegerLiteral lit Int32
instance (Assert (lit <=? $(minBoundAsNat @Int64)) (NegativeIntegerTooLargeError lit Int64 $(minBoundAsNat @Int64) $(maxBoundAsNat @Int64))) => SafeNegativeIntegerLiteral lit Int64

-- Not clear what to do with edge cases:
instance TypeError ('Text "Float unsupported by `safe-literals`." ':$$: 'Text "Potential fix: use rational notation, e.g. " ':<>: ShowType lit ':<>: 'Text ".0.") => SafeNegativeIntegerLiteral lit Float
instance TypeError ('Text "Double unsupported by `safe-literals`." ':$$: 'Text "Potential fix: use rational notation, e.g. " ':<>: ShowType lit ':<>: 'Text ".0.") => SafeNegativeIntegerLiteral lit Double

instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Complex a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Ratio a)

instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Max a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Min a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Identity a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Down a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Product a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Sum a)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Const a b)
instance (SafeNegativeIntegerLiteral lit a) => SafeNegativeIntegerLiteral lit (Ap f a)

safeNegativeIntegerLiteral :: (SafeNegativeIntegerLiteral lit a) => a -> a
safeNegativeIntegerLiteral = id
