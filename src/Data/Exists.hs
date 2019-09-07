module Data.Exists where

import GHC.Exts (Constraint)

data Exists c where
  Exists :: { fromExists :: c a } -> Exists c

data ExistsC (f :: k -> *) (constraint :: k -> Constraint) where
  ExistsC :: constraint a => f a -> ExistsC f constraint
