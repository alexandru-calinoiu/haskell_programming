module BinaryTree where

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
    Just (l, b, r) -> Node (unfold f l) b (unfold f r)
    Nothing        -> Leaf

treeBuilder :: Integer -> BinaryTree Integer
treeBuilder n = unfold f 0
    where f a
            | a == n = Nothing
            | otherwise = Just (a + 1, a, a + 1)
