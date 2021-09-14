module FixError where

a = foldr (++) [] ["woot", "WOOT", "woot"]
b = foldr max 'a' "fear is the little death"
c = foldr (&&) True [True, False]
d = foldr (flip (||)) False [False, True]
e = foldl (\b a -> b ++ show a) "" [1..5]
f = foldr (\ _ x -> x) 'a' [1..5]
g = foldr const '0' "tacos"
h = foldl const 0 "burritos"
i = foldl const 'z' [1..5]
