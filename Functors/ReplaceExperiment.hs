module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplaceWithP :: Functor f => f a -> f Char
liftedReplaceWithP = fmap replaceWithP

liftedReplaceWithP' :: [Maybe [Char]] -> [Char]
liftedReplaceWithP' = liftedReplaceWithP

twiceLifted :: (Functor f, Functor f') => f (f' a) -> f (f' Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f, Functor f', Functor f'') => f (f' (f'' a)) -> f (f' (f'' Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main = do
    putStr "replaceWithP' lms:"
    print (replaceWithP' lms)

    putStr "liftedReplaceWithP lms:"
    print (liftedReplaceWithP lms)

    putStr "twiceLifted lms:"
    print (twiceLifted lms)

    putStr "thriceLifted lms:"
    print (thriceLifted lms)

