{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-
Created       : 2014 Mar 06 (Thu) 17:12:50 by Harold Carr.
Last Modified : 2014 Mar 16 (Sun) 18:26:20 by Harold Carr.
-}

module BitlyClientTH where

import           BitlyClientCommon
import           Data.Char           (toLower)
import           Language.Haskell.TH

mk :: Name -> Q [Dec]
mk name = do
    decl <- reify name
    return [ mkFun decl]
  where
    stripQualifier = tail . dropWhile ('.'/=)
    kluge x = x ++ "KLUGE"

    mkFun (TyConI (DataD _ _ _ vcs _)) = FunD (mkName "mkReqUrl") (mkRecs vcs)
    mkFun                            _ = error "unexpected"

    mkRecs ((RecC name0 fields):xs) =
        let vcName  = show name0
            urlParm = map toLower $ stripQualifier vcName
            varPs   = map (\(nam,_,_) -> VarP (mkName (kluge (stripQualifier (show nam))))) fields
        in  (Clause
                [ConP (mkName vcName) varPs]
                (NormalB (AppE (AppE (VarE (mkName "BitlyClientTH.mru")) (LitE (StringL (map toLower urlParm))))
                          (AppE (VarE (mkName "concat"))
                           (ListE  (mkFlds fields) ))))
                []) : mkRecs xs
    mkRecs _ = []

    zrName  = mkName "BitlyClientTH.zr"

    mkFlds ((nam,_,typ):xs) =
        let fldName = stripQualifier (show nam)
        in  (AppE (AppE (VarE zrName) (LitE (StringL fldName))) ) (mkTyp (mkName (kluge fldName)) typ) : mkFlds xs
    mkFlds _ = []

    mkTyp fn (AppT ListT    (ConT _)) =                                                   VarE fn
    mkTyp fn                (ConT _)  =                                            ListE [VarE fn]
    mkTyp fn (AppT (ConT _) (ConT _)) = AppE (VarE (mkName "BitlyClientTH.showNothing")) (VarE fn)
    mkTyp _ _ = error "unexpected"

{-
mkReqUrl (Shorten "u" "d")
mkReqUrl (Expand ["u1","u2"] ["h1"])
mkReqUrl (LinkEdit "linkValue" (Just "maybeTitleValue") Nothing (Just True) (Just 3) Nothing ["edit1","edit2"])
-}

zr :: String -> [a] -> [(String,a)]
zr = zip . repeat

showNothing :: Show a => Maybe a -> [String]
showNothing Nothing  = []
showNothing (Just x) = [show x]

mru :: String -> [(String,String)] -> String
mru op p = bitlyApiV3 ++ op ++ "?" ++ (urlEncodeVars p)

{-
[FunD mkReqUrl_0 [Clause [ConP BitlyClient.RequestExpand [VarP s_1,VarP h_2]]
                  (NormalB (AppE (AppE (VarE BitlyClientTH.mru)
                                  (LitE (StringL "expand")))
                            (InfixE
                             (Just (AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "shortUrl"))) (VarE s_1)))
                             (VarE GHC.Base.++)
                             (Just (AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "hash"))) (VarE h_2))))))
                  []
                 ]
  ]

[FunD mkReqUrl_3 [Clause [ConP BitlyClient.RequestShorten [VarP l_4,VarP d_5]]
                  (NormalB (AppE (AppE (VarE BitlyClientTH.mru)
                                  (LitE (StringL "shorten")))
                            (InfixE
                             (Just (AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "longUrl"))) (ListE [VarE l_4])))
                             (VarE GHC.Base.++)
                             (Just (AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "domain"))) (ListE [VarE d_5]))))))
                  []]]


[FunD mkReqUrl_0 [Clause [ConP BitlyClient.RequestShorten [VarP l_1,VarP d_2]]
                  (NormalB (AppE (AppE (VarE BitlyClientTH.mru)
                                  (LitE (StringL "shorten")))
                            (AppE (AppE (VarE GHC.Base.++)
                                   (AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "longUrl"))) (ListE [VarE l_1])))
                             (AppE (AppE (VarE BitlyClientTH.zr) (LitE (StringL "domain"))) (ListE [VarE d_2])))))
                  []]]
-}
-- End of file.

