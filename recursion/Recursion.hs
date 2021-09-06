module Recursion where

sum' :: (Num a, Eq a) => a -> a
sum' 0 = 0
sum' n = n + sum'(n - 1)

mult' :: Integral a => a -> a -> a
mult' m 1  = m
mult' m m' = m + mult' m (m' - 1)

divideBy :: Integral a => a -> a -> DivideResult a
divideBy num den = go (abs num) (abs den) 0
    where go n d count
            | d == 0 = DivideByZero
            | n < d = divideResult (count, n) num den
            | otherwise = go (n - d) d (count + 1)

divideResult :: Integral a => (a, a) -> a -> a -> DivideResult a
divideResult (count, n) num den
    | (num < 0 && den < 0) || (num > 0 && den > 0) = Result count n
    | num < 0 || den < 0 = Result (negate count) n

data DivideResult a =
    Result a a
    | DivideByZero
    deriving Show

mc91 :: (Ord a, Num a) => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 . mc91 $ n + 11
