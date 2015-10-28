module Main(_mainSiette) where
-- siette imports
import Test.QuickCheck((==>),Property)
import SietteLib(qCheck,_doChecks_,Test_(QCheck_,Example_))

-- student's imports
-- test's imports
import Test.QuickCheck
-- extra code
p1_mcd x y = d /= 0 ==> mcd (div x d) (div y d) == 1
  where d = mcd x y
p2_mcd x y m = mcd (m * x) (m * y) == abs m * mcd x y
p3_mcd y q r = mcd x y == mcd y r
  where x = y * q + r
p4_mcd x y = mcd x y == gcd x y

-- tests
_mainSiette = _doChecks_ ts
	where
		ts = 
			[ Example_ ((mcd 30 75) == (15)) "mcd 30 75" "15" (show (mcd 30 75)) Nothing
			, Example_ ((mcd 56 63) == (7)) "mcd 56 63" "7" (show (mcd 56 63)) Nothing
			, QCheck_ (qCheck (p1_mcd :: Integer -> Integer -> Property)) "p1_mcd :: Integer -> Integer -> Property" Nothing
			, QCheck_ (qCheck (p2_mcd :: Integer -> Integer -> Integer -> Bool)) "p2_mcd :: Integer -> Integer -> Integer -> Bool" Nothing
			, QCheck_ (qCheck (p3_mcd :: Integer -> Integer -> Integer -> Bool)) "p3_mcd :: Integer -> Integer -> Integer -> Bool" Nothing
			, QCheck_ (qCheck (p4_mcd :: Integer -> Integer -> Bool)) "p4_mcd :: Integer -> Integer -> Bool" Nothing
			]

-- student's sol
 
mcd :: (Integral a) => a -> a -> a
mcd x y = mcd' (abs x) (abs y)
  where mcd' 0 y = y
        mcd' x 0 = x
        mcd' x y = maximum [d | d <- divisores x, d `elem` divisores y]
 
divideA :: (Integral a) => a -> a -> Bool
divideA x y = y `mod` x == 0
 
divisores :: (Integral a) => a -> [a]
divisores 0 = [1 ..]
divisores x = [d | d <- [1 .. x], d `divideA` x]

