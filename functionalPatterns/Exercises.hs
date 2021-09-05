module Exercises where

f :: a -> a
f x = x

tensDigit :: Integral a => a -> a
tensDigit x = d
    where (d, _) = x `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
    True  -> y
    False -> x

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
    | b = x
    | not b = y


g :: (a -> b) -> (a, c) -> (b, c)
g atob (a, c) = (atob a, c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
    print (roundTrip 4 :: Integer)
    print (id 4)
