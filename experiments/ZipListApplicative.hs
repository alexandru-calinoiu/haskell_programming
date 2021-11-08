module ZipListApplicative where

import           Test.QuickCheck          (Arbitrary (arbitrary))
import           Test.QuickCheck.Checkers (EqProp ((=-=)), eq)

newtype ZipList' a =
    ZipList' [a]
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where
            xs' = let (ZipList' l) = xs
                  in take 3000 l
            ys' = let (ZipList' l) = ys
                  in take 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = do
        ZipList' <$> arbitrary

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Semigroup (ZipList' a) where
    (<>) (ZipList' x) (ZipList' y) = ZipList' (x <> y)

instance Applicative ZipList' where
    pure a = ZipList' [a]
    (<*>) (ZipList' _) (ZipList' [])          = ZipList' []
    (<*>) (ZipList' f) (ZipList' [v]) = ZipList' (fmap (\f' -> f' v) f)
    (<*>) (ZipList' []) (ZipList' _)          = ZipList' []
    (<*>) (ZipList' [f]) (ZipList' v) = ZipList' (fmap f v)
    (<*>) (ZipList' (f:fs)) (ZipList' (v:vs)) = ZipList' [f v] <> (ZipList' fs <*> ZipList' vs)

trigger :: (ZipList' String, ZipList' String, ZipList' Int)
trigger = undefined

main :: IO ()
main = do
    print (z <*> z')
    print (z <*> pure 1)
    print (pure id <*> z')
    where
        z = ZipList' [(+9), (*2), (+8)]
        z' = ZipList' ([1..3] :: [Int])
