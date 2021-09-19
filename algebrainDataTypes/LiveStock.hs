{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LiveStock where

newtype Goats = Goats Int deriving (Show, Eq)

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Tuple = Tuple (Int, Int)

instance TooMany (Int, Int) where
    tooMany (f, l) = tooMany (f + l)

instance  (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)

newtype Cows = Cows Int deriving (Eq, Show, TooMany)
