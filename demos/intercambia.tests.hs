intercambia (1,2) => (2,1) -- Comprobación sobre enteros distintos
intercambia (True,False)  => (False,True) -- Comprobación sobre booleanos distintos
intercambia (1,True) => (True,1)
intercambia ('a','b') => ('b','a')
qCheck (p_intercambia :: Int -> Int -> Bool) -- Comprueba que funciona para un par de enteros
qCheck (p_intercambia :: Char -> Char -> Bool)

--code--

p_intercambia x y = intercambia (x,y) == (y,x)
