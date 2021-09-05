{-# OPTIONS_GHC -Wall #-}

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO()
main = do
    print ((addOne . addOne . addOne . negate . addOne) 0)
