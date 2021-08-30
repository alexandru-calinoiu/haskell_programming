module Reverse where

rvrs :: String -> String
rvrs x = word3 ++ " " ++ word2 ++ " " ++ word1
    where
        word1 = take 5 x
        word2 = drop 6 (take 8 x)
        word3 = drop 9 (take 16 x)

main :: IO()
main = print $ rvrs "Curry is awesome"
