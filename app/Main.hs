module Main where

import ECCrypto
import Data.ByteString.Char8 as BS

main :: IO ()
main = do
	BS.putStrLn $ BS.pack "encrypto ::---->"
	str <- BS.getLine
	BS.putStrLn $ encrypto str