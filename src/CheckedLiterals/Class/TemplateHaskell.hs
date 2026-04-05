{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Template Haskell helpers used to derive type-level bounds.
module CheckedLiterals.Class.TemplateHaskell where

import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import Language.Haskell.TH (Q, Type, litT, numTyLit)

-- | Reify a type's @maxBound@ as a type-level natural.
maxBoundAsNat :: forall a. (Bounded a, Typeable a, Integral a) => Q Type
maxBoundAsNat
  | theMaxBound >= 0 = litT (numTyLit theMaxBound)
  | otherwise =
      fail $
        "The type "
          ++ show (typeRep (Proxy @a))
          ++ " has a negative maxBound, so it cannot be used with CheckedLiterals. The maxBound is "
          ++ show theMaxBound
          ++ "."
 where
  theMaxBound = toInteger (maxBound :: a)

-- | Reify the absolute value of a type's non-positive @minBound@ as a type-level natural.
minBoundAsNat :: forall a. (Bounded a, Integral a, Typeable a) => Q Type
minBoundAsNat
  | theMinBound <= 0 = litT (numTyLit (-theMinBound))
  | otherwise =
      fail $
        "The type "
          ++ show (typeRep (Proxy @a))
          ++ " has a positive, non-zero minBound, so it cannot be used with CheckedLiterals. The minBound is "
          ++ show theMinBound
          ++ "."
 where
  theMinBound = toInteger (minBound :: a)
