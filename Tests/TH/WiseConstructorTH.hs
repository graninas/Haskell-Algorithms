{-# LANGUAGE TemplateHaskell #-}

module WiseConstructorTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Traversable (traverse)
import Data.Char (toLower)

traceQ p x = do
    runIO $ putStr p
    runIO $ print x

ptraceQ x = runIO $ putStrLn $ pprint x

mkMapper :: String -> String -> Name -> Q [Dec]
mkMapper mapperName mappedQualificator typeName = do
    clauses <- reify typeName >>= (mkMapAll mappedQualificator)
    d <- funD (mkName mapperName) clauses
    ptraceQ d
    return [d]

mkMapAll :: String -> Info -> Q [ClauseQ]
mkMapAll mappedQualificator i = case i of
    TyConI (DataD _ n _ cons _) -> return $ map (mkMapOne mappedQualificator) cons
    
mkMapOne :: String -> Con -> ClauseQ
mkMapOne q c | null q    = mkMapOne' [] c
             | otherwise = mkMapOne' (q ++ ".") c
  where
    mkMapOne' qualificator (NormalC n fs) = do
        let (n1:ns) = nameBase n
        let mappedFuncName = mkName $ qualificator ++ (toLower n1 : ns) :: Name
        args <- sequence $ replicate (length fs) $ newName "x" :: Q [Name]
        let qargs = map varP args
        let eargs = map varE args
        let apExpr = appsE (varE mappedFuncName : eargs)
        clause [conP n qargs] (normalB apExpr) []



