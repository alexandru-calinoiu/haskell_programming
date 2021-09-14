module Exercises where

stops = "pbtdkg"
vowels = "aeiou"

tuples :: [(Char, Char, Char)]
tuples = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

tuples' :: [(Char, Char, Char)]
tuples' = [(x, y, z) | x <- stops, x == 'p', y <- vowels, z <- stops]

seekritFunc :: [Char] -> Double
seekritFunc x =
    fromIntegral (sum (map length (words x))) /
        fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr = foldl (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myElm :: Eq a => a -> [a] -> Bool
myElm x = foldr (\a b -> a == x || b) False

myElm' :: Eq a => a -> [a] -> Bool
myElm' x = myAny (==x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then b ++ [a] else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs
