module Apl1 where

import           Control.Applicative (ZipList (ZipList), liftA2)

instance Monoid a => Monoid (ZipList a) where
    mempty = ZipList []
    mappend = liftA2 mappend
