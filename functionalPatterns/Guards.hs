{-# OPTIONS_GHC -Wall #-}

module Guards where

myAbs :: Integer -> Integer
myAbs x
    | x < 0 = negate x
    | otherwise = x

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
    | a^power + b^power == c^power = "RIGHT ON"
    | otherwise = "not"
    where power = 2 :: Integer

pal :: Eq a => [a] -> Bool
pal xs
    | xs == reverse xs = True
    | otherwise = False

numbers :: (Ord a, Num a) => a -> a
numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1
    | otherwise = 0 -- ?
