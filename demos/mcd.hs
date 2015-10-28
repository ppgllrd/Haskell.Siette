mcd :: (Integral a) => a -> a -> a
mcd x y = mcd' (abs x) (abs y)
  where
    mcd' 0 y = y
    mcd' x 0 = x
    mcd' x y = maximum [ d | d <- divisores x, d `elem` divisores y ]

divideA :: (Integral a) => a -> a -> Bool
x `divideA` y = y `mod` x == 0

divisores :: (Integral a) => a -> [a]
divisores 0 = [1..]
divisores x = [ d | d <- [1..x], d `divideA` x ]
