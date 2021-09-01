module Parametricity where

id :: a -> a
id x = x

h :: a -> a -> a
h x _ = x
h _ y = y

g :: a -> b -> b
g x y = y
