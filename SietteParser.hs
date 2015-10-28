--------------------------------------------------------------------------------
-- Extracts functions and operators declarations from a Haskell
-- source file
--
-- Pepe Gallardo, 2015
--
--------------------------------------------------------------------------------

module SietteParser ( Identifier
                    , parse
                    , getDeclarations
                    , declaration2String

                    , extractImports
                    , importToString
                    ) where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax
import Data.Map hiding (map)


type Identifier = String


  -- fn is a haskell file name and xs is a haskell program.
  -- Returns parsed module
parse :: FilePath -> String -> HsModule
parse fn xs = do
      case parseModuleWithMode (ParseMode {parseFilename= fn}) xs of
        ParseOk hsModule -> hsModule
        ParseFailed srcLoc msg -> error $ concat [ "\n"++msg
                                                 , "\nFile: "++(srcFilename srcLoc)
                                                 , "\nLine: "++show (srcLine srcLoc)
                                                 , ", Column: "++show (srcColumn srcLoc)
                                                 ]

-- program text corresponding to a parsed declaration
declaration2String :: HsDecl -> String
declaration2String decl = prettyPrint decl

-- Annotates each declaration with its identifier
name :: HsDecl -> [ (Identifier, HsDecl) ]
name ts@(HsTypeSig _ hsNames _)         = [ (toIdentifier hsNm, ts) | hsNm <- hsNames ]
name fb@(HsFunBind xs)                  = [ (nameOf (head xs),  fb) ]
name pb@(HsPatBind _ (HsPVar pvar) _ _) = [ (toIdentifier pvar, pb) ]
name id@(HsInfixDecl _ _ _ hsOps)       = [ (toIdentifier hsOp, id) | hsOp <- hsOps ]
name _                                  = [ ]

nameOf :: HsMatch -> Identifier
nameOf (HsMatch srcLoc hsName hsPats hsRhs hsDecls) = toIdentifier hsName

-- Groups declarations with same identifiers
group :: [ (Identifier, HsDecl) ] -> Map Identifier [HsDecl]
group xs = unions [ singleton id' [ decl | (id,decl) <- xs, id == id' ] | id' <- ids ]
  where
    ids = map fst xs

-- Extracts different declared entities and their code
-- as dictionary of declarations in program and their code
getDeclarations :: HsModule -> Map Identifier [HsDecl]
getDeclarations hsModule = group . concat . map name . getModuleDecls $ hsModule

-- Declarations in parsed module
getModuleDecls :: HsModule -> [HsDecl]
getModuleDecls (HsModule srcLoc modul maybeHsExportSpecs hsImportDecls hsDecls) = hsDecls

class ToIdentifier a where
  toIdentifier :: a -> Identifier

instance ToIdentifier HsName where
  toIdentifier (HsSymbol xs) = xs
  toIdentifier (HsIdent xs) = xs

instance ToIdentifier HsOp where
  toIdentifier (HsVarOp xs) = toIdentifier xs
  toIdentifier (HsConOp xs) = toIdentifier xs

extractImports :: HsModule -> ([HsImportDecl], [HsDecl])
extractImports (HsModule srcLoc modul maybeHsExportSpecs hsImportDecls hsDecls) = ( hsImportDecls, hsDecls )

importToString :: HsImportDecl -> String
importToString hsImportDecl = prettyPrint hsImportDecl



{-
main :: IO ()
main = do
  let fn = "Pepe.hs"
  xs <- readFile fn
  let d = getDeclarations fn xs
  mapM_ doPut . toList $ d
  return ()


  doPut :: (Identifier,[HsDecl]) -> IO ()
  doPut (id, decls) = do
    putStrLn (replicate 80 '-')
    putStrLn id
    putStrLn $ declarations2String decls

process :: String -> IO (Dict, Dict)
process xs = do
  case parseModule xs of
    ParseOk ok -> let
                    modDecls = getModuleDecls ok
                    types = unions . map getTypeSigs $ modDecls
                    defs = unions . map getBindings $ modDecls
                  in
                    return (types, defs)
    err        -> error ("Parse error on file:\n"++show err)


getTypeSigs :: HsDecl -> Dict
getTypeSigs ts@(HsTypeSig _ hsNames _) = unions [ singleton (toIdentifier hsNm) ts | hsNm <- hsNames ]
getTypeSigs _                          = empty

getBindings :: HsDecl -> Dict
getBindings b@(HsFunBind xs)                                 = singleton (nameOf (head xs)) b
getBindings b@(HsPatBind srcLoc (HsPVar pvar) hsRhs hsDecls) = singleton (toIdentifier pvar) b
getBindings _                                                = empty


getIdents (HsFunBind xs)                  = map getIdent' xs
getIdents (HsPatBind _ (HsPVar pvar) _ _) = [toIdentifier pvar]

getIdent' (HsMatch srcLoc hsName hsPats hsRhs hsDecls) = toIdentifier hsName





isHsFunBind :: HsDecl -> Bool
isHsFunBind (HsFunBind xs)  = True
isHsFunBind _               = False

isHsPatBind :: HsDecl -> Bool
isHsPatBind (HsPatBind srcLoc hsPat hsRhs hsDecls) = True
isHsPatBind _                                      = False



isHsBinding :: HsDecl -> Bool
isHsBinding (HsFunBind xs)                         = True
isHsBinding (HsPatBind srcLoc hsPat hsRhs hsDecls) = True
isHsBinding _                                      = False

nameBinding :: HsDecl -> (String, HsDecl)
nameBinding b@(HsFunBind xs)                                 = (nameOf (head xs), b)
nameBinding b@(HsPatBind srcLoc (HsPVar pvar) hsRhs hsDecls) = (toIdentifier pvar, b)

-}
