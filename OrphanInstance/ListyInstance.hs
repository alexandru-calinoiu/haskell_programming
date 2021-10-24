module ListyInstance where

import           Data.Monoid
import           Listy       (Listy (Listy))

instance Semigroup a => Semigroup (Listy a) where
    (<>) (Listy l) (Listy l') =
        Listy $ mappend l l'

instance Monoid (Listy a) where
    mempty = Listy []

