import Test.QuickCheck
import Data.List( head
                , tail
                )

intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)
