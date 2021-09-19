module Exercises where

import           Data.Char (chr, ord)
import           Data.List

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _          = False
isSubseqOf _ []          = False
isSubseqOf xs ys'@(y:ys) = isPrefixOf  xs ys' || isSubseqOf xs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\a -> (a, capitalizeWord a)) . words

capitalizeWord :: String -> String
capitalizeWord s@(x:xs)
    | x == ' ' = ' ' : capitalizeWord xs
    | ord x > 97 = chr (ord x - 32) : xs
    | otherwise = s

capitalizeSentence :: String -> String
capitalizeSentence = concatMap (\s -> capitalizeWord s ++ ".") . sentences

sentences :: String -> [String]
sentences s = case break (=='.') s of
    (a, ['.']) -> [a]
    (a, '.':b) -> a : sentences b
    (a, "")    -> [a]
