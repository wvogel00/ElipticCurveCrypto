module ECCrypto where

import qualified Data.ByteString.Char8 as BS

type ECurve = (Integer, Integer, Integer, Integer)
data ECCrypto = ECCrypto{x :: Integer, y :: Integer, curve :: ECurve, p :: Integer} deriving Eq

instance Num ECCrypto where
	a + b = ECCrypto{
					x = ((x a) + (x b)) `mod` (p a) , 
					y = ((y a) + (y b)) `mod` (p a) , 
					curve = curve a , 
					p = p a}
	a - b = ECCrypto{
					x = ((x a) - (x b)) `mod` (p a) , 
					y = ((y a) - (y b)) `mod` (p a) , 
					curve = curve a , 
					p = p a}
	a * b = ECCrypto{
					x = ((x a) * (x b)) `mod` (p a) , 
					y = ((y a) * (y b)) `mod` (p a) , 
					curve = curve a , 
					p = p a}
	negate a = a{x = negate (x a), y = negate (y a)}
	abs a = a{x = abs (x a), y = abs (y a)}
	fromInteger a = undefined

instance Fractional ECCrypto where
	a / b = ECCrypto{
					x = ((x a) * revSrc (x b) (p b)) `mod` (p a) , 
					y = ((y a) * revSrc (y b) (p b)) `mod` (p a) , 
					curve = curve a , 
					p = p a}
	recip = undefined
	fromRational = undefined

instance Show ECCrypto where
	show a = "(x,y)=(" ++ show (x a) ++ "," ++ show (y a) ++ ")\t E:"
				++ showPolynomial (curve a) ++ "\t , (modulo is " ++ show (p a) ++ ")"

showPolynomial (a,b,c,d) = concat.map f $ zip [a,b,c,d] [3,2,1,0] where
		f (0,0) = "\b"
		f (k,0) = show k
		f (0,_) = ""
		f (k,1) = if k == 1 then "x+" else show k ++ "x+"
		f (1,n) = "x^" ++ show n ++ "+"
		f (k,n) = show k ++ "x^" ++ show n ++ "+"

-- calc reverse source
revSrc :: Integer -> Integer -> Integer
revSrc v p = head $ filter (\n -> n*v `mod` p == 1) [1..]

-----------------------------------------------------------------
------ Encrypto -------------------------------------------------
-----------------------------------------------------------------

encrypto :: BS.ByteString -> BS.ByteString
encrypto str = str