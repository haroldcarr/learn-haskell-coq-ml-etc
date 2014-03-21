{-
Created       : 2014 Mar 06 (Thu) 17:12:50 by Harold Carr.
Last Modified : 2014 Mar 21 (Fri) 16:07:53 by Harold Carr.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module BitlyClientTH where

import           BitlyClientCommon
import           Data.Char           (toLower)
import           Data.List           (stripPrefix)
import           Data.Maybe          (fromJust)
import           Data.String.Utils
import           Language.Haskell.TH

mk :: Name -> Q [Dec]
mk name = do
    decl <- reify name
    return [ mkFun decl]
  where
    uniquify x     = x ++ "KLUGE"

    mkFun (TyConI (DataD _ tcName _ valueConstructors _)) = FunD (mkName "makeRequestUrl") (map (\vc -> valC tcName vc) valueConstructors)
    mkFun                                               _ = error "unexpected"

    valC tcName (RecC name0 fields) =
        let urlOp = mkOpName tcName name0
            varPs = map (\(nam,_,_) -> VarP (mkName (uniquify (stripQualifier nam)))) fields
        in  Clause
                [ConP name0 varPs]
                (NormalB (AppE (AppE (VarE (mkName "mru")) (LitE (StringL urlOp)))
                          (AppE (VarE (mkName "concat")) (ListE (map field fields))  )))
                []
    valC _ _ = error "unexpected"

    field (nam,_,typ) =
        let fieldName = stripQualifier nam
        in AppE (AppE (VarE (mkName "zr")) (LitE (StringL fieldName))) (ty (mkName (uniquify fieldName)) typ)

    -- TODO: Figure out how to make the types at _ explicit
    -- [String]
    ty fn (AppT ListT    (ConT _)) =                    VarE fn
    -- String
    ty fn                (ConT _)  =             ListE [VarE fn]
    -- Maybe x
    ty fn (AppT (ConT _) (ConT x)) = let sn = mkName (if (show x) == "GHC.Base.String" then "showStringNothing" else "showNothing")
                                     in AppE (VarE sn) (VarE fn)
    ty _  _                        = error "unexpected"

-- turn
--      "BitlyClient.LinkEditRequest"      into "linkedit"
--      "BitlyClient.Link_SL_LookupRequest into "link/lookup"
mkOpName :: Show a => a -> a -> String
mkOpName tcName vcName = urlOp
  where
    urlOp0 = map toLower (reverse (fromJust (stripPrefix (reverse (stripQualifier tcName)) (reverse (stripQualifier vcName)))))
    urlOp = replace "_sl_" "/" urlOp0

stripQualifier :: Show a => a -> String
stripQualifier = tail . dropWhile ('.'/=) . show

zr :: String -> [a] -> [(String,a)]
zr = zip . repeat

showNothing       :: Show a => Maybe a      -> [String]
showNothing       Nothing  = []
showNothing       (Just x) = [show x]

showStringNothing ::           Maybe String -> [String]
showStringNothing Nothing  = []
showStringNothing (Just x) = [x]

mru :: String -> [(String,String)] -> String
mru op p = bitlyApiV3 ++ op ++ "?" ++ urlEncodeVars p

-- End of file.
