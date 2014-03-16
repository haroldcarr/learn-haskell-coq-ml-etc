{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-
Created       : 2014 Mar 06 (Thu) 17:12:50 by Harold Carr.
Last Modified : 2014 Mar 16 (Sun) 16:54:00 by Harold Carr.
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
    mkFun (TyConI (DataD _ _ _ vcs _)) = FunD (mkName "mkReqUrl") (mkRecs vcs)
    mkFun                            _ = error "unexpected"
    mkRecs ((RecC name0 fields):xs) =
        let stripQualifier = tail . dropWhile ('.'/=)
            vcName  = show name0
            urlParm = map toLower $ stripQualifier vcName
            zrName  = mkName "BitlyClientTH.zr"
            varPs   = map (\(nam,_,_) -> VarP (mkName (stripQualifier (show nam)))) fields
        in  [Clause [ConP (mkName vcName) varPs]
             (NormalB (AppE (AppE (VarE (mkName "BitlyClientTH.mru")) (LitE (StringL (map toLower urlParm))))
                       (AppE (VarE (mkName "concat"))
                        (ListE
                         [(AppE (AppE (VarE zrName) (LitE (StringL "shortUrl"))) (VarE $ mkName "shortUrl"))
                         ,(AppE (AppE (VarE zrName) (LitE (StringL "hash")))     (VarE $ mkName "hash"))
                         ]))))
             []]
    mkRecs _ = error "unexpected"

zr :: String -> [a] -> [(String,a)]
zr = zip . repeat

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

