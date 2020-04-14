module Main where

import ECCrypto
import Data.Char (chr, ord)

secretkey_a = 15

main :: IO ()
main = do
  putStrLn "encrypto ::---->"
  n <- getLine
  let encoded = encrypto defaultPublickey defaultRank secretkey_a.map read.words $ n
  putStrLn $ "encrypto :=" ++ (concat.map show $ snd encoded)
  let decodedStr = decrypto encoded defaultRank
  putStrLn $ "encrypto :=" ++ (concat.map show $ decodedStr)