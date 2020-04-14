module ECCrypto where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe

type Modulo = Integer
data ResidueRingInteger = ResidueRingInteger Integer Modulo deriving (Eq, Show)

toRing :: Integer -> Modulo -> ResidueRingInteger
toRing v n = ResidueRingInteger v n

getv :: ResidueRingInteger -> Integer
getn :: ResidueRingInteger -> Modulo
getv (ResidueRingInteger v _) = v
getn (ResidueRingInteger _ n) = n

multi :: Integer -> ResidueRingInteger -> ResidueRingInteger
multi a x = ResidueRingInteger ((a*getv x) `mod` getn x) (getn x)

instance Num ResidueRingInteger where
    a + b = if getn a == getn b then ResidueRingInteger ((getv a+getv b) `mod` getn a) (getn a) else undefined
    a - b = if getn a == getn b then ResidueRingInteger ((getv a-getv b) `mod` getn a) (getn a) else undefined
    a * b = if getn a == getn b then ResidueRingInteger ((getv a*getv b) `mod` getn a) (getn a) else undefined
    negate a = ResidueRingInteger ((getn a)-(getv a)) (getn a)
    abs a = ResidueRingInteger (getv a `mod` getn a) (getn a)
    fromInteger v = undefined

instance Fractional ResidueRingInteger where
	a / b = ResidueRingInteger ((getv a * (getv $ revSrc b)) `mod` getn a) (getn a)
	recip = undefined
	fromRational = undefined

-- calc reverse source
revSrc :: ResidueRingInteger -> ResidueRingInteger
revSrc a = ResidueRingInteger v (getn a) where
	v = head $ filter (\n -> n*(getv a) `mod` (getn a) == 1) [1..]

type ECurve = (ResidueRingInteger, ResidueRingInteger, ResidueRingInteger, ResidueRingInteger)
data ECCrypto = ECCrypto{x :: ResidueRingInteger, y :: ResidueRingInteger, curve :: ECurve} deriving Eq

get0, get1, get2, get3 :: ECurve-> ResidueRingInteger
get0 (_,_,_,d) = d
get1 (_,_,c,_) = c
get2 (_,b,_,_) = b
get3 (a,_,_,_) = a

(+++) :: ECCrypto -> ECCrypto -> ECCrypto
a +++ b = a{x = x3, y = -lambda*x3 - nu} where
			x3 = if a == b
				then lambda*lambda-multi 2 (x a)
				else lambda * lambda -(x a) - (x b)
			lambda = if a == b
				then (multi 3 (x a)*(x a) + (get1 $ curve a) ) / (multi 2 (y a))
				else ((y b)-(y a)) /((x b)-(x a))
			nu = (y a) - lambda * (x a)

instance Show ECCrypto where
	show a = "(x,y)=(" ++ show (getv $ x a) ++ "," ++ show (getv $ y a) ++ ")\t E:"
				++ showPolynomial (curve a) ++ "\t , (modulo is " ++ show (getn $ x a) ++ ")"

showPolynomial :: ECurve -> String
showPolynomial (a,b,c,d) = concat.map f $ zip (map getv [a,b,c,d]) [3,2,1,0] where
		f (0,0) = "\b"
		f (k,0) = show k
		f (0,_) = ""
		f (k,1) = if k == 1 then "x+" else show k ++ "x+"
		f (1,n) = "x^" ++ show n ++ "+"
		f (k,n) = show k ++ "x^" ++ show n ++ "+"


-----------------------------------------------------------------
------ Encrypto / Decrypto --------------------------------------
-----------------------------------------------------------------

defaultN = 31
defaultRank = 41
defaultEC = (toRing 1 defaultN, toRing 0 defaultN, toRing 2 defaultN, toRing 17 defaultN)
defaultECCrypto = ECCrypto{x = toRing 10 defaultN, y = toRing 13 defaultN, curve = defaultEC}

type SecretKey = Integer
secretkey_b = 6 :: SecretKey

default_B = multiplyOnEC defaultECCrypto secretkey_b

type Rank = Integer
type PublicKey = (ECCrypto, ECCrypto)
defaultPublickey = (defaultECCrypto, default_B) :: PublicKey

multiplyOnEC :: ECCrypto -> Integer -> ECCrypto
multiplyOnEC ec = fromJust.snd.foldl multiplyOnEC' (ec, Nothing).toBin where
	multiplyOnEC' :: (ECCrypto, Maybe ECCrypto) -> Integer -> (ECCrypto, Maybe ECCrypto)
	multiplyOnEC' (nP,result) 0 = (nP+++nP, result)
	multiplyOnEC' (nP,Nothing) 1 = (nP+++nP, Just nP)
	multiplyOnEC' (nP,Just result) 1 = (nP+++nP, Just $ result +++ nP)

-- convert base number, 10 to 2
toBin 0 = []
toBin n = mod n 2 : toBin (div n 2)

takePos eccrypto = (x eccrypto, y eccrypto)

encrypto :: PublicKey -> Rank -> SecretKey -> [Integer] -> (ECCrypto, [Integer])
encrypto (_P,bP) rank secretkey message = (aP, map encrypto' message) where
	encrypto' = flip mod rank.( + (getv $ x abP))
	aP = multiplyOnEC _P secretkey
	abP = multiplyOnEC bP secretkey

decrypto :: (ECCrypto, [Integer]) -> Rank -> [Integer]
decrypto (aP, message) rank = map decrypto' message where
	decrypto' m = (m - (getv $ x abP)) `mod` rank
	abP = multiplyOnEC aP secretkey_b