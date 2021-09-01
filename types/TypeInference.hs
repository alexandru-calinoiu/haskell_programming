module TypeInference where

myConcat :: [Char] -> [Char]
myConcat x = x ++ " yo"

myMul :: Fractional a => a -> a
myMul x = (x / 3) * 5

myTake :: Int -> [Char]
myTake x = take x "hey you"

myCom :: Int -> Bool
myCom x = x > length [1..10]

myAlph :: Char -> Bool
myAlph x = x < 'c'
