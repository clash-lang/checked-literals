{-# LANGUAGE AllowAmbiguousTypes #-}

module SafeLiterals.Class.TemplateHaskell where

import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import Language.Haskell.TH (Q, Type, litT, numTyLit)

maxBoundAsNat :: forall a. (Bounded a, Typeable a, Integral a) => Q Type
maxBoundAsNat
  | theMaxBound >= 0 = litT (numTyLit theMaxBound)
  | otherwise =
      fail $
        "The type "
          ++ show (typeRep (Proxy @a))
          ++ " has a negative maxBound, so it cannot be used with SafeLiterals. The maxBound is "
          ++ show theMaxBound
          ++ "."
 where
  theMaxBound = toInteger (maxBound :: a)

minBoundAsNat :: forall a. (Bounded a, Integral a, Typeable a) => Q Type
minBoundAsNat
  | theMinBound <= 0 = litT (numTyLit (-theMinBound))
  | otherwise =
      fail $
        "The type "
          ++ show (typeRep (Proxy @a))
          ++ " has a positive, non-zero minBound, so it cannot be used with SafeLiterals. The minBound is "
          ++ show theMinBound
          ++ "."
 where
  theMinBound = toInteger (minBound :: a)
