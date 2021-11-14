module Monads.Ex1 where

import           Control.Monad (join, (>=>))

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else []

sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
    getAge "Hello! How old are you? "
