module Cipher where

import           Data.Char (chr, ord)

caesar :: Int -> String -> String
caesar x = map (shift x)

shift :: Int -> Char -> Char
shift x c
    | d > 0 = chr $ ord 'a' + m - 1
    | otherwise = chr $ ord c + x
        where (d, m) = (ord c + x) `divMod` ord 'z'

unCaesar :: Int -> String -> String
unCaesar x = map (shift (negate x))

