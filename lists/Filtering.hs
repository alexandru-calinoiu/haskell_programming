module Filtering where

multiplesOfThree = filter (\x -> (x `rem` 3) == 0) [1..30]
lengthOfMultiples = length multiplesOfThree

myFilter :: String -> [String]
myFilter s = filter isArticle $ words s

isArticle :: String -> Bool
isArticle w
    | w == "the" = False
    | w == "a" = False
    | w == "an" = False
    | otherwise = True
