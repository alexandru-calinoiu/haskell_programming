module IntegralDivision where

divideBy :: Integral a => a -> a -> (a, a)
divideBy num den = go num den 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n -d) d (count + 1)
