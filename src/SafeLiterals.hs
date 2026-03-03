module SafeLiterals (
  plugin,
  uncheckedLiteral,
  module SafeLiterals.Class,
) where

import SafeLiterals.Class
import SafeLiterals.Plugin (plugin)

{- | A function that can be used to bypass the safety checks of @safe-literals@.
This works for both integer and rational literals.
-}
uncheckedLiteral :: a -> a
uncheckedLiteral = id
