-- Pepe Gallardo, 2015
-- Library for integration of Haskell in Siette

module SietteLib
    ( _doChecks_
    , qCheck
    , Test_(QCheck_,Example_)
    ) where

import Test.QuickCheck( quickCheckWithResult, Args(..), stdArgs, Result(..)
                      , Property, Testable
                      )
import Control.Monad(unless)

data Test_ = QCheck_  { prop :: IO Bool
                      , text :: String
                      , comment :: Maybe String
                      }
          | Example_ { example :: Bool
                     , expr :: String
                     , expectedResult :: String
                     , realResult :: String
                     , comment :: Maybe String
                     }

data TestResult = OK
                | QCheckFailure { desc :: String }
                | ExampleFailure { desc :: String }


_doChecks_ :: [Test_] -> IO ()
_doChecks_ xs = do
  rs <- mapM doCheck_ xs
  let (qCheckErrs, exampleErrs) = classify rs
  putStrLn $ if (null qCheckErrs && null exampleErrs) then ok
                                                      else failure
  unless (null exampleErrs) $ putStr (unlines ("+ Casos de prueba con resultado incorrecto:" : exampleErrs))
  unless (null qCheckErrs) $ putStr (unlines ("+ Pruebas QuickCheck con resultado incorrecto:" : qCheckErrs))

classify :: [TestResult] -> ([String], [String])
classify []       = ([],[])
classify (r : rs) =
  let (ys,zs) = classify rs
  in case r of
    OK -> (ys,zs)
    QCheckFailure desc -> (desc:ys, zs)
    ExampleFailure desc -> (ys, desc:zs)

doCheck_ :: Test_ -> IO TestResult
doCheck_ (QCheck_ prop text comment) = do
  b <- prop
  return $ if b then OK
                else QCheckFailure $ unwords [ text, getComment "  --" comment ]
doCheck_ (Example_ example expr expectedResult realResult comment) = do
  let b = example
  return $ if b then OK
                else ExampleFailure $ unwords [ expr, "=>", realResult
                                              , "  -- Se esperaba como resultado"
                                              , expectedResult
                                              ] ++ getComment "." comment

getComment :: String -> Maybe String -> String
getComment ys Nothing   = ""
getComment ys (Just xs) = ys++" "++xs

ok = "+++OK"
failure = "+++FALLO"

qCheck :: (Testable prop) => prop -> IO Bool
qCheck prop = do
  r <- quickCheckWithResult stdArgs{chatty=False} prop
  case r of
    Success{} -> return True
    GaveUp{}  -> return True
    _         -> return False
