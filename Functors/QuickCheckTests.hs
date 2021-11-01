module QuickCheckTests where

import           Test.QuickCheck (Arbitrary (arbitrary), quickCheck)

functorIdentiy :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentiy f = id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap (g . f) x == fmap (g . f) x

f :: [Int] -> Bool
f = functorIdentiy

c :: (Functor f, Eq (f a), Num a) => f a -> Bool
c = functorCompose (+1) (*2)

li :: [Int] -> Bool
li x = c  (x :: [Int])

--

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary =
      Identity <$> arbitrary

--

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        return (Pair a a)

--

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        Two a <$> arbitrary

--

data Posibility a =
    LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Posibility where
    fmap _ LolNope     = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

--

data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First x)  = First x
    fmap f (Second x) = Second (f x)

--

newtype Wrap f a =
    Wrap (f a)
    deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

--

getInt :: IO Int
getInt = fmap read getLine

main = do
    quickCheck f
    quickCheck li
    --
    quickCheck (\x -> functorIdentiy (x :: Identity Int))
    quickCheck (\x -> c (x :: Identity Int))
    --
    quickCheck (\x -> functorIdentiy (x :: Pair Int))
    quickCheck (\x -> c (x :: Pair Int))
    --
    quickCheck (\x -> functorIdentiy (x :: Two Int Int))
    quickCheck (\x -> c (x :: Two Int Int))
