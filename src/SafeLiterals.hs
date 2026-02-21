module SafeLiterals
  ( plugin
  , uncheckedLiteral
  , module SafeLiterals.Class
  ) where

import SafeLiterals.Class
import SafeLiterals.Plugin (plugin)

-- | A function that can be used to bypass the safety checks of @safe-literals@.
uncheckedLiteral :: a -> a
uncheckedLiteral = id
