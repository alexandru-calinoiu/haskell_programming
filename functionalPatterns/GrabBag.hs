module GrabBag where

mTh x y z = x * y * z

mTh1 x y   = \z -> x * y * z

mTh2 x = \y -> \z -> x * y * z

mTh3 = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
    True  -> f n
    False -> n
    where f = (+ 1)

addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x y -> (if x > y then y else x) + 5

mFlip f = \x -> \y -> f y x
mFlip' f x y = f y x
