{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CheckedLiterals.Nums.Fixed (
  Fixed (..),
  SFixed,
  UFixed,
) where

import CheckedLiterals (
  CheckedNegativeIntegerLiteral,
  CheckedNegativeRationalLiteral,
  CheckedPositiveIntegerLiteral,
  CheckedPositiveRationalLiteral,
 )
import CheckedLiterals.Class.Rational.TypeNats (IsPowerOfTwo)
import CheckedLiterals.Nums.Signed (Signed (..))
import CheckedLiterals.Nums.Unsigned (Unsigned (..))
import Data.Bits (Bits (..), shiftL, shiftR, (.&.))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Ratio (denominator, numerator, (%))
import Data.Type.Bool (If)
import GHC.TypeError (Assert, ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeLits (KnownNat, Nat, natVal, type Div, type (+), type (-), type (<=?), type (^))
import GHC.TypeLits.Extra (CLog)

{- | Fixed-point number

Where:

* @rep@ is the underlying representation (Signed or Unsigned)
* @int@ is the number of bits used to represent the integer part
* @frac@ is the number of bits used to represent the fractional part
-}
newtype Fixed (rep :: Nat -> Type) (int :: Nat) (frac :: Nat)
  = Fixed {unFixed :: rep (int + frac)}

{- | Signed fixed-point number with @int@ integer bits (including sign bit)
and @frac@ fractional bits
-}
type SFixed = Fixed Signed

-- | Unsigned fixed-point number with @int@ integer bits and @frac@ fractional bits
type UFixed = Fixed Unsigned

instance (KnownNat frac, Integral (rep (int + frac))) => Show (Fixed rep int frac) where
  show (Fixed fRep) = i ++ "." ++ fracStr
   where
    nF = fromInteger (natVal (Proxy @frac)) :: Int
    fRepI = toInteger fRep
    fRepI_abs = abs fRepI
    i =
      if fRepI < 0
        then '-' : show (fRepI_abs `shiftR` nF)
        else show (fRepI `shiftR` nF)
    nom =
      if fRepI < 0
        then fRepI_abs .&. ((2 ^ nF) - 1)
        else fRepI .&. ((2 ^ nF) - 1)
    denom = 2 ^ nF
    fracStr = padZeros nF (show (numerator r'))
     where
      r = nom % denom
      -- Multiply by 10^nF to get decimal representation
      r' =
        iterate
          ( \x ->
              let n = numerator x * 10
                  d = denominator x
               in n % d
          )
          r
          !! nF
    padZeros n str = replicate (n - length str) '0' ++ str

instance (KnownNat frac, Integral (rep (int + frac)), Eq (rep (int + frac))) => Eq (Fixed rep int frac) where
  Fixed a == Fixed b = a == b

instance (KnownNat frac, Integral (rep (int + frac)), Ord (rep (int + frac))) => Ord (Fixed rep int frac) where
  Fixed a `compare` Fixed b = a `compare` b

-- | Num instance for Fixed - operations saturate on overflow
instance
  ( KnownNat frac
  , KnownNat int
  , Integral (rep (int + frac))
  , Bounded (rep (int + frac))
  , Bits (rep (int + frac))
  ) =>
  Num (Fixed rep int frac)
  where
  Fixed a + Fixed b = Fixed (fromInteger sat)
   where
    res = toInteger a + toInteger b
    maxB = toInteger (maxBound :: rep (int + frac))
    minB = toInteger (minBound :: rep (int + frac))
    sat = max minB (min maxB res)

  Fixed a * Fixed b = Fixed (fromInteger sat)
   where
    nF = fromInteger (natVal (Proxy @frac)) :: Int
    res = (toInteger a * toInteger b) `shiftR` nF
    maxB = toInteger (maxBound :: rep (int + frac))
    minB = toInteger (minBound :: rep (int + frac))
    sat = max minB (min maxB res)

  Fixed a - Fixed b = Fixed (fromInteger sat)
   where
    res = toInteger a - toInteger b
    maxB = toInteger (maxBound :: rep (int + frac))
    minB = toInteger (minBound :: rep (int + frac))
    sat = max minB (min maxB res)

  negate (Fixed a) = Fixed (negate a)

  abs (Fixed a) = Fixed (abs a)

  signum (Fixed a)
    | a == 0 = 0
    | a < 0 = -1
    | otherwise = 1

  fromInteger i = Fixed (fromInteger sat)
   where
    nF = fromInteger (natVal (Proxy @frac)) :: Int
    res = i `shiftL` nF
    maxB = toInteger (maxBound :: rep (int + frac))
    minB = toInteger (minBound :: rep (int + frac))
    sat = max minB (min maxB res)

-- | Fractional instance for Fixed - division and rational conversion
instance
  ( KnownNat frac
  , KnownNat int
  , Integral (rep (int + frac))
  , Bounded (rep (int + frac))
  , Bits (rep (int + frac))
  ) =>
  Fractional (Fixed rep int frac)
  where
  Fixed a / Fixed b = Fixed (fromInteger sat)
   where
    nF = fromInteger (natVal (Proxy @frac)) :: Int
    -- Shift numerator left by frac bits before division for precision
    num = toInteger a `shiftL` nF
    res = num `quot` toInteger b
    maxB = toInteger (maxBound :: rep (int + frac))
    minB = toInteger (minBound :: rep (int + frac))
    sat = max minB (min maxB res)

  recip (Fixed a) = Fixed (fromInteger sat)
   where
    nF = fromInteger (natVal (Proxy @frac)) :: Int
    -- 1.0 in fixed point is 1 << frac
    one = 1 `shiftL` nF :: Integer
    -- (1 << frac) / a in fixed point needs another shift
    num = one `shiftL` nF
    res = num `quot` toInteger a
    maxB = toInteger (maxBound :: rep (int + frac))
    minB = toInteger (minBound :: rep (int + frac))
    sat = max minB (min maxB res)

  fromRational r = Fixed (fromInteger sat)
   where
    nF = fromInteger (natVal (Proxy @frac)) :: Int
    n = numerator r `shiftL` (2 * nF)
    d = denominator r `shiftL` nF
    res = n `quot` d
    maxB = toInteger (maxBound :: rep (int + frac))
    minB = toInteger (minBound :: rep (int + frac))
    sat = max minB (min maxB res)

type PositiveUnsignedError strLit lit int typ maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " is (potentially) out of bounds."
        ':$$: 'Text "Note: integer part needs at least "
          ':<>: 'ShowType (CLog 2 lit + 1)
          ':<>: 'Text " bit(s)."
        ':$$: 'Text "Possible fix: add a constraint: "
          ':<>: 'ShowType (CLog 2 lit + 1)
          ':<>: 'Text " <= "
          ':<>: 'ShowType int
          ':<>: 'Text "."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

instance
  ( Assert
      (If (lit <=? 0) (lit <=? 0) (CLog 2 (lit + 1) <=? int))
      (PositiveUnsignedError (ShowType lit) lit int (UFixed int frac) ((2 ^ int) - 1))
  ) =>
  CheckedPositiveIntegerLiteral lit (UFixed int frac)

type NegativeUnsignedError strLit typ maxVal =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " is out of bounds, because UFixed cannot represent negative numbers."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

instance
  (NegativeUnsignedError ('Text "-" ':<>: 'ShowType lit) (UFixed int frac) ((2 ^ int) - 1)) =>
  CheckedNegativeIntegerLiteral lit (UFixed int frac)

type FixedPointNotPow2Error strLit den typ =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " cannot be represented exactly by "
        ':<>: 'ShowType typ
        ':<>: 'Text "."
        ':$$: 'Text "The reduced denominator "
          ':<>: 'ShowType den
          ':<>: 'Text " is not a power of 2."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

type FixedPointNotEnoughFracError strLit den frac typ =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " cannot be represented exactly by "
        ':<>: 'ShowType typ
        ':<>: 'Text "."
        ':$$: 'Text "The fractional part needs at least "
          ':<>: 'ShowType (CLog 2 den)
          ':<>: 'Text " bit(s)."
        ':$$: 'Text "Possible fix: add a constraint: "
          ':<>: 'ShowType (CLog 2 den)
          ':<>: 'Text " <= "
          ':<>: 'ShowType frac
          ':<>: 'Text "."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

type family
  CheckFrac (isPow2 :: Bool) (strLit :: ErrorMessage) (den :: Nat) (frac :: Nat) (typ :: Type) ::
    Constraint
  where
  CheckFrac 'False strLit den frac typ = FixedPointNotPow2Error strLit den typ
  CheckFrac 'True strLit den frac typ =
    Assert
      (CLog 2 den <=? frac)
      (FixedPointNotEnoughFracError strLit den frac typ)

instance
  ( Assert
      (If (Div num den <=? 0) (Div num den <=? 0) (CLog 2 (Div num den + 1) <=? int))
      (PositiveUnsignedError ('Text str) (Div num den) int (UFixed int frac) ((2 ^ int) - 1))
  , CheckFrac (IsPowerOfTwo den) ('Text str) den frac (UFixed int frac)
  ) =>
  CheckedPositiveRationalLiteral str num den (UFixed int frac)

instance
  (NegativeUnsignedError ('Text str) (UFixed int frac) ((2 ^ int) - 1)) =>
  CheckedNegativeRationalLiteral str num den (UFixed int frac)

type PositiveSignedError strLit lit int typ =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " is (potentially) out of bounds."
        ':$$: 'Text "Note: integer part needs at least "
          ':<>: 'ShowType (CLog 2 (lit + 1) + 1)
          ':<>: 'Text " bit(s), including sign bit."
        ':$$: 'Text "Possible fix: add a constraint: "
          ':<>: 'ShowType (CLog 2 (lit + 1) + 1)
          ':<>: 'Text " <= "
          ':<>: 'ShowType int
          ':<>: 'Text "."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

instance
  ( Assert
      (If (lit <=? 0) (lit <=? 0) (CLog 2 (lit + 1) + 1 <=? int))
      (PositiveSignedError (ShowType lit) lit int (SFixed int frac))
  ) =>
  CheckedPositiveIntegerLiteral lit (SFixed int frac)

type NegativeSignedError strLit lit int typ =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " is (potentially) out of bounds."
        ':$$: 'Text "Note: integer part needs at least "
          ':<>: 'ShowType (CLog 2 lit + 1)
          ':<>: 'Text " bit(s), including sign bit."
        ':$$: 'Text "Possible fix: add a constraint: "
          ':<>: 'ShowType (CLog 2 lit + 1)
          ':<>: 'Text " <= "
          ':<>: 'ShowType int
          ':<>: 'Text "."
        ':$$: 'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."
    )

instance
  ( Assert
      (If (lit <=? 0) (lit <=? 0) (CLog 2 lit + 1 <=? int))
      (NegativeSignedError ('Text "-" ':<>: 'ShowType lit) lit int (SFixed int frac))
  ) =>
  CheckedNegativeIntegerLiteral lit (SFixed int frac)

instance
  ( Assert
      (If (Div num den <=? 0) (Div num den <=? 0) (CLog 2 (Div num den + 1) + 1 <=? int))
      (PositiveSignedError ('Text str) (Div num den) int (SFixed int frac))
  , CheckFrac (IsPowerOfTwo den) ('Text str) den frac (SFixed int frac)
  ) =>
  CheckedPositiveRationalLiteral str num den (SFixed int frac)

instance
  ( Assert
      (If (Div num den <=? 0) (Div num den <=? 0) (CLog 2 (Div num den) + 1 <=? int))
      (NegativeSignedError ('Text str) (Div num den) int (SFixed int frac))
  , CheckFrac (IsPowerOfTwo den) ('Text str) den frac (SFixed int frac)
  ) =>
  CheckedNegativeRationalLiteral str num den (SFixed int frac)
