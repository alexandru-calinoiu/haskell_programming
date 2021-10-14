module Experiments where

import qualified Data.List       as L
import           Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, elements,
                                  frequency, quickCheck)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements  [1, 2, 3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements  [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    frequency [ (1, return Nothing)
              , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

half :: Double -> Double
half x = x / 2

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = 2 * half x == x

runQcForHalf :: IO ()
runQcForHalf = quickCheck prop_halfIdentity

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, _)       = (Just y, x >= y)

prop_listOrdered :: [Int] -> Bool
prop_listOrdered = listOrdered . L.sort

runQcForSort :: IO ()
runQcForSort = quickCheck prop_listOrdered

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
    x + y == y + x

runPlusQc :: IO ()
runPlusQc = do
    quickCheck plusAssociative
    quickCheck plusCommutative

prop_quotRem :: Integral a => a -> a -> Bool
prop_quotRem x y = quot x y * y + rem x y == x


