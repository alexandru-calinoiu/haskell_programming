module ListAplicative where

data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons x l) = Cons (f x) (fmap f l)

instance Semigroup (List a) where
    (<>) Nil ys         = ys
    (<>) (Cons x xs) ys = Cons x $ xs <> ys

instance Applicative List where
    pure a = Cons a Nil
    (<*>) (Cons _ _) Nil         = Nil
    (<*>) Nil (Cons _ _)         = Nil
    (<*>) Nil Nil                = Nil
    (<*>) (Cons f l) (Cons x l') = Cons (f x) (fmap f l') <> (l <*> Cons x l')

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

-- concat' fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f a = concat' $ fmap f a

main :: IO ()
main = do
    print (fs <*> vs)
    print (flatMap f xs)
    where
        fs = Cons (+1) (Cons (*2) Nil)
        vs = Cons (1 :: Int) (Cons 2 Nil)

        toMyList = foldr Cons Nil
        xs = toMyList [1, 2, 3]
        c = Cons
        f x = x `c` ((9 :: Int) `c` Nil)

