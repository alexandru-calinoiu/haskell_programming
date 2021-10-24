module Monoid.Optional where

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada (Only a)      = Only a
  (<>) (Only a) Nada      = Only a
  (<>) Nada Nada          = Nada
  (<>) (Only a) (Only a') = Only (a <> a')

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
