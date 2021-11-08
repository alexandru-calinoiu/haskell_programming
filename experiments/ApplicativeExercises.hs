module ApplicativeExercises where

import           Test.QuickCheck           (Arbitrary)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import           Test.QuickCheck.Checkers  (EqProp ((=-=)), eq, quickBatch)
import           Test.QuickCheck.Classes   (applicative)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (<*>) (Pair f f') (Pair x y) = Pair (f x) (f' y)

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        return (Pair a a)

----

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    (<*>) (Two a f') (Two a' b) = Two (a <> a') (f' b)

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        Two a <$> arbitrary

----

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (<*>) (Three x y f) (Three x' y' z) = Three (x <> x') (y <> y') (f z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        Three a b <$> arbitrary

----

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' x x' x'' y) = Four' x x' x'' (f y)

instance (Monoid a) => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    (<*>) (Four' x y z f) (Four' x' y' z' t) = Four' (x <> x') (y <> y') (z <> z') (f t)

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        Four' a a a <$> arbitrary

----

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos xs ys zs = (,,) <$> xs <*> ys <*> zs

---

main :: IO ()
main = do
    quickBatch $ applicative (undefined :: Pair (String, String, Int))
    quickBatch $ applicative (undefined :: Two String (String, String, Int))
    quickBatch $ applicative (undefined :: Three String String (String, String, Int))
    quickBatch $ applicative (undefined :: Four' String (String, String, Int))
    print (combos vowels stops vowels)

