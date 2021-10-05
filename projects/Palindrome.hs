module Palindrome where

import           Control.Monad (forever)
import           Data.Char     (isAlpha, toLower)
import           System.Exit   (exitSuccess)
import           System.IO     (BufferMode (NoBuffering), hSetBuffering, stdout)

removeNonAlpha :: String -> String
removeNonAlpha = filter isAlpha

lower :: String -> String
lower = fmap toLower

isPalindrom :: String -> Bool
isPalindrom w = cleaned == reverse cleaned
    where cleaned = lower $ removeNonAlpha w

main :: IO ()
main = forever $ do
  hSetBuffering stdout NoBuffering
  putStr "What do you want me to check? "
  line <- getLine

  if isPalindrom line
      then (do
            putStrLn "It's a palindrome!"
            exitSuccess)
      else putStrLn "Try again!"
