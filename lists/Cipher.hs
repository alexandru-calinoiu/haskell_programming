module Cipher where

import           Data.Char (chr, ord)
import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)


caesar :: Int -> String -> String
caesar x = map (shift x)

shift :: Int -> Char -> Char
shift x c
    | d > 0 = chr $ ord 'a' + m - 1
    | otherwise = chr $ ord c + x
        where (d, m) = (ord c + x) `divMod` ord 'z'

unCaesar :: Int -> String -> String
unCaesar x = map (shift (negate x))

encrypt :: IO ()
encrypt = do
  hSetBuffering stdout NoBuffering
  putStr "Message to encrypt: "
  sentenceToCode <- getLine
  putStr "Shift by: "
  shiftBy <- getLine
  putStrLn $ "Encoded phrase is: " ++ caesar (read shiftBy::Int) sentenceToCode
  return ()


decrypt :: IO ()
decrypt = do
  hSetBuffering stdout NoBuffering
  putStr "Message to decrypt: "
  sentenceToCode <- getLine
  putStr "Shift by: "
  shiftBy <- getLine
  putStrLn $ "Decoded phrase is: " ++ unCaesar (read shiftBy::Int) sentenceToCode
  return ()
