{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CheckedLiterals.Class.Rational.TypeNats where

import Data.Type.Bool (If)
import GHC.TypeLits

type family IsPowerOfTwo (n :: Nat) :: Bool where
  IsPowerOfTwo 0 = 'False
  IsPowerOfTwo 1 = 'True
  IsPowerOfTwo n = If (Mod n 2 <=? 0) (IsPowerOfTwo (Div n 2)) 'False
