{-# OPTIONS_GHC -Wall #-}

module Case where

functionC :: Ord p => p -> p -> p
functionC x y = case x > y of
    True  -> x
    False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = case isEven of
    True  -> n + 2
    False -> n
    where isEven = even n

nums :: (Ord a, Num a, Num p) => a -> p
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0
