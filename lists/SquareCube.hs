module SquareCube where

import           Data.Bool (bool)

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

tuplets = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]

ifThenMap = map (\x -> bool x (-x) (x == 3)) [1..10]
