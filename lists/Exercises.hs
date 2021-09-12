module Exercises where

import           Data.Char

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

capitalize' :: String -> String
capitalize' (x:xs)
    | null xs = [toUpper x]
    | otherwise = toUpper x : capitalize' xs

firstCapitalized = toUpper . head


myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
    | x = True
    | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
    | f x = True
    | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys)
    | x == y = True
    | otherwise = myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (==x)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squish' :: [[a]] -> [a]
squish' = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) =  f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
