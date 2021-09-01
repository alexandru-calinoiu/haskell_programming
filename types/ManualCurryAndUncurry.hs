module ManualCurryAndUncurry where

nonsens :: Bool -> Integer
nonsens True  = 805
nonsens False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + nonsens b

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + nonsens b

anonnymous :: Integer -> Bool -> Integer
anonnymous = \i b -> i + nonsens b

anonNested :: Integer  -> Bool -> Integer
anonNested = \i -> \b -> i + nonsens b
