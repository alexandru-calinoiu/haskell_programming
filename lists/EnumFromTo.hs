module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

eft :: (Ord a, Enum a) => a -> a -> [a]
eft start end
    | start > end = []
    | start == end = [start]
    | otherwise = start : eft (succ start) end
