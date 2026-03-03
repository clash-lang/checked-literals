module Data.Ratio.Extra where

import Data.Ratio qualified as Ratio

{- | Show a 'Rational' in fixed-point notation, without using scientific notation.

>>> showFixedPoint (1 % 2)
"0.5"
>>> showFixedPoint (-0.1)
"-0.1"
>>> showFixedPoint 10
"10.0"
>>> showFixedPoint 1.154646000
"1.154646"
-}
showFixedPoint :: Rational -> String
showFixedPoint rational
  | numerator < 0 = "-" <> result
  | otherwise = result
 where
  result = show whole <> "." <> concat (go0 fractional)
  (whole, fractional) = abs numerator `quotRem` denominator
  numerator = Ratio.numerator rational
  denominator = Ratio.denominator rational

  go0 0 = ["0"]
  go0 i = go1 i

  go1 0 = []
  go1 i = show w : go1 f
   where
    (w, f) = (10 * i) `quotRem` denominator
