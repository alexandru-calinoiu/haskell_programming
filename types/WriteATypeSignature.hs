module WriteATypeSignature where

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = x > y

funcionS :: (a, b) -> b
funcionS (x, y) = y

co :: (b -> c) -> (a -> b) -> a -> c
co bToc aTob a = bToc (aTob a)


a' :: (a -> b) -> a -> b
a' aTob = aTob
