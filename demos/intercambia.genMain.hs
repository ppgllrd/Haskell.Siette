module Main(_mainSiette) where
-- siette imports
import Test.QuickCheck((==>),Property)
import SietteLib(qCheck,_doChecks_,Test_(QCheck_,Example_))

-- student's imports
import Test.QuickCheck
import Data.List (head, tail)
-- test's imports
-- extra code
p_intercambia x y = intercambia (x, y) == (y, x)

-- tests
_mainSiette = _doChecks_ ts
	where
		ts = 
			[ Example_ ((intercambia (1,2)) == ((2,1))) "intercambia (1,2)" "(2,1)" (show (intercambia (1,2))) (Just "Comprobaci\243n sobre enteros distintos")
			, Example_ ((intercambia (True,False) ) == ((False,True))) "intercambia (True,False)" "(False,True)" (show (intercambia (True,False) )) (Just "Comprobaci\243n sobre booleanos distintos")
			, Example_ ((intercambia (1,True)) == ((True,1))) "intercambia (1,True)" "(True,1)" (show (intercambia (1,True))) Nothing
			, Example_ ((intercambia ('a','b')) == (('b','a'))) "intercambia ('a','b')" "('b','a')" (show (intercambia ('a','b'))) Nothing
			, QCheck_ (qCheck (p_intercambia :: Int -> Int -> Bool)) "p_intercambia :: Int -> Int -> Bool" (Just "Comprueba que funciona para un par de enteros")
			, QCheck_ (qCheck (p_intercambia :: Char -> Char -> Bool)) "p_intercambia :: Char -> Char -> Bool" Nothing
			]

-- student's sol
 
intercambia :: (a, b) -> (b, a)
intercambia (x, y) = (y, x)

