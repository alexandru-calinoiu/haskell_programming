{-# LANGUAGE NoMonomorphismRestriction #-}

example :: Num p => p
example = 1

example2 :: Num a => a
example2 = (* 9) 6

example3 :: Num a => (a, [Char])
example3 = head [(0, "doge"), (1, "kitteh")]

