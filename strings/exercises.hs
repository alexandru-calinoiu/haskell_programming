module Exercises where

trim :: String -> String
trim = tail

yell :: String -> String
yell s = s ++ "!e"

forth :: String -> Char
forth s = s !! 4

dropItLikeIsHot :: String -> String
dropItLikeIsHot = drop 9

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = str !! x
    where str = "Curry is awesome"
