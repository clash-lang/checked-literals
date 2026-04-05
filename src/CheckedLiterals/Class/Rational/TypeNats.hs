{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Small type-level helpers used by rational literal checks.
module CheckedLiterals.Class.Rational.TypeNats where

import Data.Type.Bool (If)
import GHC.TypeLits

-- | Type-level predicate that checks whether a natural is a power of two.
type family IsPowerOfTwo (n :: Nat) :: Bool where
  IsPowerOfTwo 0 = 'False
  IsPowerOfTwo 1 = 'True
  IsPowerOfTwo n = If (Mod n 2 <=? 0) (IsPowerOfTwo (Div n 2)) 'False
