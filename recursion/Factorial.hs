module Factorial where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n - 1)

fact :: [Integer]
fact = scanl (*) 1 [1..]
factN :: Int -> Integer
factN x = fact !! x
