{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE TemplateHaskell          #-}

module Support where

-- import           Data.List                  (splitAt)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Foreign

-- https://wiki.haskell.org/Foreign_Function_Interface
importDoubleToDouble :: String -> ExpQ
importDoubleToDouble fname = do
  n <- newName fname
  d <- forImpD CCall unsafe fname n [t|Double -> Double|]
  addTopDecls [d]
  [|$(varE n)|]

data Foo = Bar | Baz deriving (Show)

xxx :: Foo -> (String, String)
xxx x = splitAt 2 (show x)


data OpaqueSignatureParamsG1
newtype SignatureParamsG1 = SignatureParamsG1 (Ptr OpaqueSignatureParamsG1) deriving (Eq, Show)
data OpaqueRng
newtype Rng = Rng (Ptr OpaqueRng) deriving (Eq, Show)

www :: Q Exp
www  = [| bbsp_sp_new :: Ptr OpaqueRng -> Int -> IO (Ptr OpaqueSignatureParamsG1) |]

zzz :: Q Exp
zzz  = runQ www

mkFFO1U_ :: Q [Dec]
mkFFO1U_  = [d|
  foreign import ccall unsafe "bbsp_sp_print"
    bbsp_sp_print :: Ptr OpaqueSignatureParamsG1 -> IO ()
  sigParamsPrint :: SignatureParamsG1 -> IO ()
  sigParamsPrint (SignatureParamsG1 sp) = bbsp_sp_print sp
  |]

mkFFO1U :: String -> String -> String -> Q [Dec]
mkFFO1U  dname0 cname0 hname0 = do
  var <- newName "var"
  let odname = mkName ("Opaque" ++ dname0)
      dname  = mkName dname0
      cname  = mkName cname0
      hname  = mkName hname0
  pure
    [ ForeignD
      (ImportF CCall Unsafe ("static " ++ cname0)
        cname
        (AppT
          (AppT ArrowT (AppT (ConT (mkName "Ptr")) (ConT odname)))
          (AppT (ConT (mkName "IO")) (TupleT 0))))
    , SigD hname
      (AppT
        (AppT ArrowT (ConT dname))
        (AppT (ConT (mkName "IO")) (TupleT 0)))
    , FunD hname
      [Clause
        [ConP dname [] [VarP var]]
        (NormalB (AppE (VarE cname) (VarE var))) []]
    ]

mkPrinter :: String -> String -> String -> Q [Dec]
mkPrinter dname0 cname0 hname0 = mkFFO1U dname0 (cname0 ++ "_print") (hname0 ++ "Print")

mkFreer :: String -> String -> String -> Q [Dec]
mkFreer   dname0 cname0 hname0 = mkFFO1U dname0 (cname0 ++ "_free")  (hname0 ++ "Free")

mkPrinterAndFreer :: String -> String -> String -> Q [Dec]
mkPrinterAndFreer dname0 cname0 hname0 = do
  p <- mkPrinter dname0 cname0 hname0
  f <- mkFreer   dname0 cname0 hname0
  pure (p ++ f)

mkPF :: String -> String -> String -> Q [Dec]
mkPF  = mkPrinterAndFreer

{-
cabal repl

:set -XTemplateHaskell
:m + Language.Haskell.TH
import Foreign
-}

{-
--------------------------------------------------
THIS WORKS

class (Eq a) => Countable a where
  count :: a -> a

data Proxy t = Proxy deriving ( Bounded , Read )

deriveCountableSimple :: Name -> Q [Dec]
deriveCountableSimple name = [d|
  instance Countable $a where
    count Proxy = fromIntegral $
      1 + fromEnum (maxBound :: $a) - fromEnum (minBound :: $a)
  |]
  where
    a = conT name

--------------------------------------------------
THIS DOES NOT WORK

mk :: String -> Q [Dec]
mk basename = [d|
  data OpaqueFoo
  newtype Foo = Foo (Ptr OpaqueFoo)
    deriving (Eq, Show)
  |]
 where
  oname  = conT (mkName ("Opaque" ++ basename)) :: Q Type
  ntname = conT (mkName basename)               :: Q Type
-}

{-
:set -XTemplateHaskell
:m + Language.Haskell.TH
import Docknetwork.THSupport
-- $(mk (mkName "OpaqueFoo") (mkName "Foo"))
-}

{-
:{
runQ [d|
  foreign import ccall unsafe "aaaa"
    aaaa :: Ptr OpaqueRng -> Int -> Ptr OpaqueSignatureParamsG1 -> IO (Ptr OpaqueSignatureParamsG1)
  aaAA :: Rng -> Int -> SignatureParamsG1 -> IO SignatureParamsG1
  aaAA (Rng rng) i (SignatureParamsG1 sp) = SignatureParamsG1 <$>  aaaa rng i sp

  foreign import ccall unsafe "bbbb"
    bbbb :: Int -> IO Bool
  bbBB :: Int -> IO Bool
  bbBB i = bbbb i

  foreign import ccall unsafe "cccc"
    cccc :: Int -> IO ()
  ccCC :: Int -> IO ()
  ccCC i = cccc i
  |]
:}
-}
{-
[ ForeignD
  (ImportF CCall Unsafe "static aaaa" aaaa_28
    (AppT (AppT ArrowT (AppT (ConT GHC.Ptr.Ptr) (ConT Support.OpaqueRng)))
      (AppT (AppT ArrowT (ConT GHC.Types.Int))
        (AppT (AppT ArrowT (AppT (ConT GHC.Ptr.Ptr) (ConT Support.OpaqueSignatureParamsG1)))
          (AppT (ConT GHC.Types.IO) (AppT (ConT GHC.Ptr.Ptr)
                                      (ConT Support.OpaqueSignatureParamsG1)))))))
  , SigD aaAA_25
    (AppT (AppT ArrowT (ConT Support.Rng))
      (AppT (AppT ArrowT (ConT GHC.Types.Int))
        (AppT (AppT ArrowT (ConT Support.SignatureParamsG1))
          (AppT (ConT GHC.Types.IO) (ConT Support.SignatureParamsG1)))))
  , FunD aaAA_25
    [ Clause
      [ ConP Support.Rng [] [VarP rng_29]
      , VarP i_30
      , ConP Support.SignatureParamsG1 [] [VarP sp_31]]
      (NormalB
        (InfixE
          (Just (ConE Support.SignatureParamsG1))
          (VarE Data.Functor.<$>)
          (Just (AppE
                  (AppE
                    (AppE (VarE aaaa_28) (VarE rng_29))
                    (VarE i_30))
                  (VarE sp_31)))))
      []]

  , ForeignD
    (ImportF CCall Unsafe "static bbbb" bbbb_27
      (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (ConT GHC.Types.IO) (ConT GHC.Types.Bool))))
  , SigD bbBB_24
    (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (ConT GHC.Types.IO) (ConT GHC.Types.Bool)))
  , FunD bbBB_24
    [ Clause
      [VarP i_32]
      (NormalB (AppE (VarE bbbb_27) (VarE i_32)))
      []]
  , ForeignD
    (ImportF CCall Unsafe "static cccc" cccc_26
      (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (ConT GHC.Types.IO) (TupleT 0))))
  , SigD ccCC_23
    (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (ConT GHC.Types.IO) (TupleT 0)))
  , FunD ccCC_23
    [ Clause
      [VarP i_33]
      (NormalB (AppE (VarE cccc_26) (VarE i_33)))
      []]
  ]
-}


{-
runQ [d| data OpaqueSignatureParamsG1 |]
runQ [d| newtype SignatureParamsG1 = SignatureParamsG1 (Ptr OpaqueSignatureParamsG1) deriving (Eq, Show) |]

:{
runQ [d|
  foreign import ccall unsafe "bbbb"
    bbbb :: IO Bool
  bbBB :: IO Bool
  bbBB = bbbb
  |]
:}


:{
xx = runQ [d|
       data OpaqueSignatureParamsG1
       newtype SignatureParamsG1 = SignatureParamsG1 (Ptr OpaqueSignatureParamsG1) deriving (Eq, Show)
     |]
:}

[ForeignD (ImportF CCall Unsafe "static aaaa" aaaa_5 (AppT (AppT ArrowT (AppT (ConT GHC.Ptr.Ptr) (ConT Support.OpaqueRng))) (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (AppT ArrowT (AppT (ConT GHC.Ptr.Ptr) (ConT Support.OpaqueSignatureParamsG1))) (AppT (ConT GHC.Types.IO) (AppT (ConT GHC.Ptr.Ptr) (ConT Support.OpaqueSignatureParamsG1))))))),SigD aaAA_2 (AppT (AppT ArrowT (ConT Support.Rng)) (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (AppT ArrowT (ConT Support.SignatureParamsG1)) (AppT (ConT GHC.Types.IO) (ConT Support.SignatureParamsG1))))),FunD aaAA_2 [Clause [ConP Support.Rng [] [VarP rng_6],VarP i_7,ConP Support.SignatureParamsG1 [] [VarP sp_8]] (NormalB (InfixE (Just (ConE Support.SignatureParamsG1)) (VarE Data.Functor.<$>) (Just (AppE (AppE (VarE rng_6) (VarE i_7)) (VarE sp_8))))) []],ForeignD (ImportF CCall Unsafe "static bbbb" bbbb_4 (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (ConT GHC.Types.IO) (ConT GHC.Types.Bool)))),SigD bbBB_1 (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (ConT GHC.Types.IO) (ConT GHC.Types.Bool))),FunD bbBB_1 [Clause [VarP i_9] (NormalB (AppE (VarE bbbb_4) (VarE i_9))) []],ForeignD (ImportF CCall Unsafe "static cccc" cccc_3 (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (ConT GHC.Types.IO) (TupleT 0)))),SigD ccCC_0 (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (ConT GHC.Types.IO) (TupleT 0))),FunD ccCC_0 [Clause [VarP i_10] (NormalB (AppE (VarE cccc_3) (VarE i_10))) []]]

:{
runQ [d| sigParamsNew :: Rng -> Int -> IO SignatureParamsG1
         sigParamsNew (Rng rng) i = SignatureParamsG1 <$> bbsp_sp_new rng i |]
:}

:{
runQ [d| f a b c d e f  = a b c d e f |]
:}
:{
runQ [d|
  foreign import ccall unsafe "bbsp_sp_new"
    bbsp_sp_new :: Ptr OpaqueRng -> Int -> IO (Ptr OpaqueSignatureParamsG1)
  sigParamsNew :: Rng -> Int -> IO SignatureParamsG1
  sigParamsNew (Rng rng) i = SignatureParamsG1 <$> bbsp_sp_new rng i
  |]

runQ [d| sigParamsNew :: Rng -> Int -> IO SignatureParamsG1
         sigParamsNew (Rng rng) i = SignatureParamsG1 <$> bbsp_sp_new rng i |]
:}

-------------------------

runQ [| \x -> concat [[1],[x]] |]

LamE [VarP x_15] (AppE (VarE Data.Foldable.concat) (ListE [ListE [LitE (IntegerL 1)],ListE [VarE x_15]]))

-------------------------

data Request = Request Int Float

$(stringE . show =<< reify ''Request)

"TyConI (DataD [] Ghci17.Request [] Nothing [NormalC Ghci17.Request [(Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Int),(Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Float)]] [])"

-}

-- End of file.


