{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-
Created       : 2014 Mar 06 (Thu) 17:12:50 by Harold Carr.
Last Modified : 2014 Mar 10 (Mon) 00:10:37 by Harold Carr.

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

import           Language.Haskell.TH

mk :: String -> [ (String, [ String ], [ String ]) ] -> Q [Dec]
mk tcName valueConstructors =
    return [ DataD [] (mkName tcName) [] vcs [ mkName "Eq", mkName "Show" ]]
  where
    vcs = mkVcs valueConstructors
    mkVcs ((vcName, multiStringFields, singleStringFields):t) =
        RecC (mkName $ tcName ++ vcName) (mfs multiStringFields singleStringFields) : mkVcs t
    mkVcs [] = []
    mfs m s = msf m ++ ssf s
    msf (name:t) = (mkName name, NotStrict, AppT ListT (ConT (mkName "String"))) : msf t
    msf [] = []
    ssf (name:t) = (mkName name, NotStrict,             ConT (mkName "String"))  : ssf t
    ssf [] = []


[FunD mkReqUrl_0 [Clause [ConP BitlyClient.ExpandRequest [VarP s_1,VarP h_2]]
                  (NormalB (AppE (AppE (VarE BitlyClient.mru) (LitE (StringL "expand")))
                            (InfixE
                             (Just (AppE (AppE (VarE BitlyClient.zr) (LitE (StringL "shortUrl"))) (VarE s_1)))
                             (VarE GHC.Base.++)
                             (Just (AppE (AppE (VarE BitlyClient.zr) (LitE (StringL "hash"))) (VarE h_2))))))
                  []]]

[FunD mkReqUrl_3 [Clause [ConP BitlyClient.ShortenRequest [VarP l_4,VarP d_5]]
                  (NormalB (AppE (AppE (VarE BitlyClient.mru) (LitE (StringL "shorten")))
                            (InfixE
                             (Just (AppE (AppE (VarE BitlyClient.zr) (LitE (StringL "longUrl"))) (ListE [VarE l_4])))
                             (VarE GHC.Base.++)
                             (Just (AppE (AppE (VarE BitlyClient.zr) (LitE (StringL "domain"))) (ListE [VarE d_5])))))) []]]




-- End of file.

