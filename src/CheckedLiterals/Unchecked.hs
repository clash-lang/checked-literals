module CheckedLiterals.Unchecked (uncheckedLiteral) where

-- | Identity function used to opt a single literal out of checking.
uncheckedLiteral :: a -> a
uncheckedLiteral = id
