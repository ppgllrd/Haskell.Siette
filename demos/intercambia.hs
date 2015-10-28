import Test.QuickCheck
import Data.List( head
                , tail
                )

intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)

data PP = PP

instance Eq PP where
  PP == PP = True

data PPP = PPP
