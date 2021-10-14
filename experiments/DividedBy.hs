module DividedBy where

import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
    describe "dividedBy" $ do
        it "15 divide by 3 is 5" $ do
            dividedBy 15 (3 :: Integer) `shouldBe` (5, 0)
        it " 22 divided by 4 is 5 reminder 2" $ do
            dividedBy 22 (4 :: Integer) `shouldBe` (5, 2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)
