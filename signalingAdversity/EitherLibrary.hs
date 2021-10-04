module EitherLibrary where

lefts' :: [Either a b] -> [a]
lefts' = map left . filter isLeft

lefts'' :: [Either a b] -> [a]
lefts'' x = [a | Left a <- x]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

left :: Either a b -> a
left (Left a) = a

rights' :: [Either a b] -> [b]
rights' = map right . filter isRight

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

right :: Either a b -> b
right (Right b) = b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e = case e of
        Right b -> Just (f b)
        _       -> Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToC bToC e = case e of
    Right b -> bToC b
    Left a  -> aToC a

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' bToC = either' (const Nothing) (Just . bToC)
