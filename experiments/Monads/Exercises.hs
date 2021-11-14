module Monads.Exercises where

import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import           Test.QuickCheck.Checkers  (EqProp ((=-=)), eq, quickBatch)
import           Test.QuickCheck.Classes   (applicative, functor, monad)
import           Test.QuickCheck.Gen       (frequency)

----

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg

instance EqProp (Nope a) where
    (=-=) = eq

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

----

data BahEither b a =
    PLeft a
    | PRight b
    deriving (Eq, Show)

instance Functor (BahEither a) where
    fmap f (PLeft x)  = PLeft (f x)
    fmap _ (PRight x) = PRight x

instance Applicative (BahEither a) where
    pure = PLeft
    (<*>) (PRight x) _        = PRight x
    (<*>) _ (PRight x)        = PRight x
    (<*>) (PLeft f) (PLeft x) = PLeft (f x)

instance Monad (BahEither a) where
    return = pure
    (PRight x) >>= _ = PRight x
    (PLeft x) >>= f  = f x

instance (Eq b, Eq a) => EqProp (BahEither b a) where
    (=-=) = eq

instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return (PRight a))
                  , (1, return (PLeft b))]

----

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
    return = pure
    (Identity x) >>= f = f x

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary =
        Identity <$> arbitrary

----

data List a =
    Nil
    | Const a (List a)
    deriving (Show, Eq)

append :: List a -> List a -> List a
append Nil ys         = ys
append xs Nil         = xs
append (Const x l) ys = Const x (l `append` ys)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Const a l) = Const (f a) (fmap f l)

instance Applicative List where
    pure x = Const x Nil
    Nil <*> _          = Nil
    _ <*> Nil          = Nil
    (Const f b) <*> ca = fmap f ca `append` (b <*> ca)

instance Monad List where
    return = pure
    Nil >>= _         = Nil
    (Const x l) >>= f = f x `append` (l >>= f)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return Nil)
                  , (10, return (Const x y))]

take' :: Int -> List a -> List a
take' = go
    where
        go 0 xs           = xs
        go _ Nil          = Nil
        go n (Const x xs) = Const x (go (n - 1) xs)

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = take' 3000 xs
              ys' = take' 3000 ys

----

j :: Monad m => m (m a) -> m a
j x = x >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f = fmap f

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m m' = do
    x <- m
    f x <$> m'

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _     = return []
meh (x:xs) f =  do
    x' <- f x
    xs' <- meh xs f
    return (x' : xs')

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id

main :: IO ()
main = do
    quickBatch $ functor (undefined :: Nope (Int, String, Int))
    quickBatch $ applicative (undefined :: Nope (Int, String, Int))
    quickBatch $ monad (undefined :: Nope (Int, String, Int))
    ----
    quickBatch $ functor (undefined :: BahEither (Int, String, Int) (String, String, String))
    quickBatch $ applicative (undefined :: BahEither (Int, String, Int) (String, String, String))
    quickBatch $ monad (undefined :: BahEither (Int, String, Int) (String, String, String))
    ----
    quickBatch $ functor (undefined :: Identity (Int, String, Int))
    quickBatch $ applicative (undefined :: Identity (Int, String, Int))
    quickBatch $ monad (undefined :: Identity (Int, String, Int))
    ----
    quickBatch $ functor (undefined :: List (Int, String, Int))
    quickBatch $ applicative (undefined :: List (Int, String, Int))
    quickBatch $ monad (undefined :: List (Int, String, Int))

