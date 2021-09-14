module Fibonacci where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

fibs :: [Integer]
fibs = take 20 go
    where go = 1 : scanl (+) 1 fibs

less = [x | x <- fibs, x < 100]
