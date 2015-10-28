mcd 30 75 => 15
mcd 56 63 => 7
qCheck (p1_mcd :: Integer -> Integer -> Property)
qCheck (p2_mcd :: Integer -> Integer -> Integer -> Bool)
qCheck (p3_mcd :: Integer -> Integer -> Integer -> Bool)
qCheck (p4_mcd :: Integer -> Integer -> Bool)


--code--

import Test.QuickCheck

divideA :: Integral a => a -> a -> Bool
x `divideA` y = y `mod` x == 0

divisores :: Integral a => a -> [a]
divisores 0 = [1..]
divisores x = [ d | d <- [1..x], d `divideA` x ]


p1_mcd x y = d /= 0 ==> mcd (div x d) (div y d) == 1
 where
   d = mcd x y

p2_mcd x y m = mcd (m*x) (m*y) == abs m * mcd x y

p3_mcd y q r = mcd x y == mcd y r
  where x = y*q + r

p4_mcd x y = mcd x y == gcd x y
