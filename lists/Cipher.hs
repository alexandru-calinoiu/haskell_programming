module Cipher where

import           Data.Char (chr, ord)

caesar :: Int -> String -> String
caesar x = map (shift x)

shift :: Int -> Char -> Char
shift x c
    | index >= 0 = chr $ ord 'a' + index
    | otherwise = chr $ ord 'z' + index + 1
        where index = ord c - ord 'a' + x

unCaesar :: Int -> String -> String
unCaesar x = map (shift (negate x))

