module Exercises where

awesome = ["Pauchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
