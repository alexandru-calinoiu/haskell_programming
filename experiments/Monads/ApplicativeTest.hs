module Modans.ApplicativeTest where

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "anotheng thing"

sequecing' :: IO ()
sequecing' =
    putStrLn "blah" >>
    putStrLn "anotheng thing"

sequecing'' :: IO ()
sequecing'' =
    putStrLn "blah" *>
    putStrLn "anotheng thing"

