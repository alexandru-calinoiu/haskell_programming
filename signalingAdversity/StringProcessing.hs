module StringProcessing where

import qualified Data.Maybe
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe = concatMap (\a -> replaceWord a ++ " ") . words

replaceWord :: String -> String
replaceWord s = Data.Maybe.fromMaybe "a" (notThe s)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) False 0
    where go (x:xs) wasThe r
            | wasThe && hasVowel x && xs /= [] = go xs False (r + 1)
            | wasThe && hasVowel x && null xs = r + 1
            | null xs = r
            | x == "the" = go xs True r
            | otherwise = go xs False r

hasVowel :: String -> Bool
hasVowel ('e':xs) = True
hasVowel _        = False

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel a = a `elem` vowels

isConson :: Char -> Bool
isConson a = not (isVowel a)

countVowels :: String -> Integer
countVowels s = go s 0
    where go (x:xs) c
            | isVowel x && xs /= [] = go xs (c+1)
            | isVowel x && null xs = c + 1
            | null xs = c
            | otherwise = go xs c

countConsons :: String -> Integer
countConsons = foldr (\a b -> if isConson a then b + 1 else b) 0

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
    | countVowels s > countConsons s = Nothing
    | otherwise = Just $ Word' s

data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger x = go x 0
    where go x n = case x of
                    Zero   -> 0
                    Succ s -> go s n + 1


integerToNat :: Integer -> Maybe Nat
integerToNat x = go x (Just Zero)
    where go x n
            | x < 0 = Nothing
            | x == 0 = n
            | otherwise = case n of
                Just n' -> go (x - 1) (Just (Succ n'))

integerToNat' :: Integer  -> Maybe Nat
integerToNat' x
    | x < 0 = Nothing
    | otherwise = Just $ foldr (\_ b -> Succ b) Zero [0..x - 1]
