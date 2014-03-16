{-
Created       : 2014 Mar 15 (Sat) 17:38:34 by Harold Carr.
Last Modified : 2014 Mar 15 (Sat) 22:28:21 by Harold Carr.

http://neilmitchell.blogspot.com/2013/10/haskell-type-graphs-with-uniplate-and.html
-}

import           Data.Generics.Uniplate.Data
import           Language.Haskell.Exts
import           System.Environment

main :: IO ()
main = do
    [filename] <- getArgs
    doit filename

doit :: FilePath -> IO ()
doit filename =
    writeFile "JUNK.hs" . show . convertIt . reach =<< getModule filename

getModule :: FilePath -> IO Module
getModule filename = do
    src <- readFile filename
    return $ fromParseResult $ parseModule src

reach :: Module -> [(String, [(String, [(String, Type)])])]
reach m = [ (prettyPrint name0, [ (prettyPrint rcName, [ (fName, typ)
                                                       | ([Ident fName], UnBangedTy typ) <- y ])
                                | RecDecl rcName y <- universeBi ctors])
          | DataDecl _ _ _ name0 _ ctors _ <- universeBi m]

-- convertIt :: [(String, [(String, [(String, Type)])])] -> String
-- convertIt xs = show xs

--             Request   Expand    url     String
convertIt :: [(String, [(String, [(String, Type)])])] -> [(String, [(String, [(String, Type)])])]
convertIt (("Request", vc):xs) = ("Request", convertVc vc) : convertIt xs
convertIt [] = []

convertVc :: [(t, [(t1, Type)])] -> [(t, [(t1, Type)])]
convertVc ((vcName, fields):xs) = (vcName, convertFields fields) : convertVc xs
convertVc [] = []

convertFields :: [(t, Type)] -> [(t, Type)]
convertFields ((field, typ):xs) = (field, convertTyp typ) : convertFields xs
convertFields [] = []

convertTyp :: Type -> Type
convertTyp                                 t@(TyList (TyCon (UnQual (Ident _)))) = t
convertTyp                                         t@(TyCon (UnQual (Ident _)))  = t
convertTyp t@(TyApp (TyCon (UnQual (Ident "Maybe"))) (TyCon (UnQual (Ident _)))) = t
convertTyp t = error $ "Unexpected type: " ++ show t

-- End of file.
