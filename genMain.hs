-- Pepe Gallardo, 2015
-- Compile with ghc -O2 genMain.hs -o genMain

import System.Environment(getArgs)
import Control.Monad(when, unless)
import System.Directory(doesFileExist)
import Data.List(isPrefixOf, isInfixOf, intercalate)
import qualified Data.Map as M
import Data.Char(isSpace)
import Prelude hiding (readFile)
import System.IO hiding (readFile)
import SietteParser

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ error "needs 2 parameters: <file sol> <file tests>"
  let studentFile = args !! 0
  studentCode <- readAFile studentFile
  let testFile = args !! 1
  txtTests <- readAFile testFile

  hsModStudent <- force $ parse studentFile studentCode
  let studentImports = extractImports hsModStudent
  let studentDecls = getDeclarations hsModStudent

  let (testsAndChecks,extraCode) = breakWith tkBeginCode txtTests
  let mainTxt = genMain testsAndChecks

  hsModExtra <- force $ parse testFile extraCode
  let extraCodeImports = extractImports hsModExtra
  let extraCodeDecls = getDeclarations hsModExtra

  -- mapM_ putAssoc (M.toList extraCodeDecls)

  let newExtraCodeDecls = M.difference extraCodeDecls studentDecls

  let dict2Code = unlines . concat . map (map declaration2String) . M.elems
  let newExtraCode = dict2Code newExtraCodeDecls
  let newStudentCode = dict2Code studentDecls

  let sep = replicate 80 '-'

  putStrLn . filter (/='\r') . unlines $
                     [ sietteHeader
                     , sep
                     , "-- student's imports"
                     ] ++ map importToString studentImports ++
                     [ sep
                     , "-- test's imports"
                     ] ++ map importToString extraCodeImports ++
                     [ sep
                     , "-- extra code", newExtraCode
                     , sep
                     , "-- tests", mainTxt
                     , sep
                     , "-- student's sol", newStudentCode
                     ]


sietteHeader = unlines  [ "module Main(_mainSiette) where"
                        , "-- siette imports"
                        , "import Test.QuickCheck((==>),Property)"
                        , "import SietteLib(qCheck,_doChecks_,Test_(QCheck_,Example_))"
                        ]

tkReduces = " => "
tkBeginCode = "--code--"
tkQCheck = "qCheck"
tkComment = " -- "
tkImport = "import "

readAFile :: FilePath -> IO String
readAFile fn = do
  exist <- doesFileExist fn
  unless exist $ error ("file "++fn++" does not exist")
  handle <- openFile fn ReadMode
  hSetEncoding handle utf8
  contents <- hGetContents handle
  let xs = length contents `seq` return contents
  xs
  hClose handle
  xs

force :: a -> IO a
force x = x `seq` return x

genMain :: String -> String
genMain xs = unlines [ "_mainSiette = _doChecks_ ts"
                     , "\twhere"
                     , "\t\tts = "++asList tests
                     ]
  where
    validLines = filter validLine . map (dropWhile isSpace) . lines $ xs
    tests = genTests validLines

genTests []           = []
genTests (line:xs)
  | isQuickCheck line = genQuickCheckTest line : genTests xs
  | isExample line    = genExampleTest line : genTests xs
  | otherwise         = error ("qCheck property or expr => result expected. Found: \""++line++"\" instead")

-- True if this line is a qCheck test
isQuickCheck :: String -> Bool
isQuickCheck xs = tkQCheck `isPrefixOf` xs

-- True if this line is an example test:  expr => result
isExample :: String -> Bool
isExample xs = tkReduces `isInfixOf` xs

genQuickCheckTest :: String -> String
genQuickCheckTest line = unwords [ "QCheck_", "("++ line' ++")"
                                 , show prop
                                 , mkComment comment
                                 ]
 where
   (line', comment) = breakWith tkComment line
   prop = removeParents . removeSpaces . snd $ breakWith tkQCheck line'

genExampleTest :: String -> String
genExampleTest line = unwords [ "Example_", code, show (removeSpaces expr)
                              , show (removeSpaces result)
                              , "(show ("++expr++"))"
                              , mkComment comment
                              ]
  where
    (line', comment) = breakWith tkComment line
    (expr,result) = breakWith tkReduces line'
    code = "((" ++expr ++ ") == (" ++ result ++ "))"

mkComment :: String -> String
mkComment xs
  | null xs   = "Nothing"
  | otherwise = "(Just "++show (removeSpaces xs)++")"

-- Is this a non empty line?
validLine :: String -> Bool
validLine xs = not (null xs) && any (not . isSpace) xs

asList :: [String] -> String
asList xs = indent "[ " ++ intercalate (indent ", ") xs ++ indent "]"
 where
  indent xs = "\n\t\t\t"++xs

-- Returns 2 lists: one with elements before tk and another with elements after tk
breakWith :: (Eq a) => [a] -> [a] -> ([a],[a])
breakWith tk []        = ([],[])
breakWith tk xs@(x:xs')
  | tk `isPrefixOf` xs = ([], drop (length tk) xs)
  | otherwise          = (x:ys, zs)
  where (ys,zs) = breakWith tk xs'

-- replaces all occurrences of y with ys in xs
replaceWith :: (Eq a) => a -> [a] -> [a] -> [a]
replaceWith y ys [] = []
replaceWith y ys (x:xs)
  | y == x          = ys ++ replaceWith y ys xs
  | otherwise       = x :  replaceWith y ys xs

removeSpaces :: String -> String
removeSpaces =  reverse . dropWhile isSpace. reverse . dropWhile isSpace

removeParents :: String -> String
removeParents [] = []
removeParents xs
  | head xs == '(' && last xs == ')' = tail (init xs)
  | otherwise                        = xs


{-
-- Extract imports from source file contents
-- An import is a line beginning with tkImport and nexts lines that
-- either are empty or that start with space (indented ones)
extractImports :: String -> ([String], [String])
extractImports xs = aux ls
  where
    ls = lines xs
    aux [] = ([],[])
    aux (l:ls)
      | tkImport `isPrefixOf` l = let (xs,ys) = aux ls' in (l:cont ++ xs, ys)
      | otherwise               = let (xs,ys) = aux ls in (xs, l:ys)
      where
        (cont,ls') = span (\l -> null l || isSpace (head l)) ls
-}
