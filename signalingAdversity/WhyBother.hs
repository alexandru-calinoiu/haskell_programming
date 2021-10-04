module WhyBother where

import           Data.List (foldl')

mehSum :: Num a => [a] -> a
mehSum (x:xs)
    | null xs = x
    | otherwise = x + mehSum xs

okSum :: Num a => [a] -> a
okSum = foldl' (+) 0

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
    Just (a, b) -> a : myUnfoldr f b
    Nothing     -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just(b, f b))
