{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-
Created       : 2014 Mar 06 (Thu) 17:12:50 by Harold Carr.
Last Modified : 2014 Mar 13 (Thu) 17:23:22 by Harold Carr.

data Request = ExpandRequest { shortUrl :: [String]
                             , hash     :: [String]
                             }
               | ShortenRequest { longUrl :: String
                                , domain  :: String
                                }

mk "Request" [ ("Expand" , ["shortUrl", "hash"], [])
             , ("Shorten", []                  , ["longUrl", "domain"])
             ]

-}

module BitlyClientTH where

import           BitlyClientCommon
import           Language.Haskell.TH

mk :: String -> [ (String, [ String ], [ String ]) ] -> Q [Dec]
mk tcName valueConstructors =
    return [ recs, funs ]
  where
    recs = DataD [] (mkName tcName) [] vcs [ mkName "Eq", mkName "Show" ]
    vcs = mkVcs valueConstructors
    mkVcs ((vcName, multiStringFields, singleStringFields):t) =
        RecC (mkName $ tcName ++ vcName) (mfs multiStringFields singleStringFields) : mkVcs t
    mkVcs [] = []
    mfs m s = msf m ++ ssf s
    msf (name:t) = (mkName name, NotStrict, AppT ListT (ConT (mkName "String"))) : msf t
    msf [] = []
    ssf (name:t) = (mkName name, NotStrict,             ConT (mkName "String"))  : ssf t
    ssf [] = []
    varS = mkName "s"
    varH = mkName "h"
    funs = FunD (mkName "mkReqUrl")
                [Clause [ConP (mkName "BitlyClient.RequestExpand") [VarP varS,VarP varH]]
                 (NormalB (AppE (AppE (VarE (mkName "BitlyClientTH.mru")) (LitE (StringL "expand")))
                           (InfixE
                            (Just (AppE (AppE (VarE (mkName "BitlyClientTH.zr")) (LitE (StringL "shortUrl"))) (VarE varS)))
                            (VarE (mkName "++"))
                            (Just (AppE (AppE (VarE (mkName "BitlyClientTH.zr")) (LitE (StringL "hash"))) (VarE varH))))))
                 []]

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
                  []]]

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

