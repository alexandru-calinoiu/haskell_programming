module Vigenere where

import           Data.Char (chr, ord)

encode :: String -> String -> String
encode cipher = zipWith shift (cycle cipher) . concat . words

shift :: Char -> Char -> Char
shift x y
    | d > 0 = chr $ ord 'a' + m - 1
    | otherwise = chr $ ord y + distance
        where distance = ord x - ord 'a'
              (d, m) = (ord y + distance) `divMod` ord 'z'

