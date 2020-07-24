{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module X01 where

------------------------------------------------------------------------------
import           Gauge
------------------------------------------------------------------------------
--import qualified Data.Text                 as T
--import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Typeable
import qualified Prelude
import           Protolude
import           Refined
------------------------------------------------------------------------------
{-# ANN module ("HLint: ignore Reduce duplication" :: Prelude.String) #-}
------------------------------------------------------------------------------

x01a
  :: Double -> Double -> Double
  -> (Double -> Double -> [Double] -> Gauge Double -> IO Double)
  -> IO Double
x01a incGauge incCheck mx lop =
  Gauge.new137 0.0 incGauge mx >>=
  lop incCheck mx [0.0]

l1 :: Double -> Double -> [Double] -> Gauge Double -> IO Double
l1  = fix $ \loop inc mx rs g -> do
  r <- readGauge g
  print r
  case doCheck inc (r:rs) of
    Left  e   -> panic (show e)
    Right rs' ->
      if r < mx
        then loop inc mx rs' g
        else pure r
 where
  doCheck = checkMaxFlow

x01al1Good, x01al1Bad :: IO Double
x01al1Good = x01a 1.0 1.0 10.0 l1 -- increases and correct rate
x01al1Bad  = x01a 2.0 1.0 10.0 l1 -- detects increases faster than possible

------------------------------------------------------------------------------

l2 :: Double -> Double -> [Double] -> Gauge Double -> IO Double
l2  = fix $ \loop inc mx rs g -> do
  r <- readGauge g
  print r
  let rsx = if r > 3.0 then -r:rs else r:rs
  print rsx
  case doCheck inc rsx of
    Left   e  -> panic (show e)
    Right rs' ->
      if r < mx
        then loop inc mx rs' g
        else pure r
 where
  doCheck = checkMaxFlow

x01al2Bad :: IO Double
x01al2Bad  = x01a 1.0 1.0 10.0 l2 -- does NOT detect reverse flow (causing infinite loop)

------------------------------------------------------------------------------

l3 :: Double -> Double -> [Double] -> Gauge Double -> IO Double
l3  = fix $ \loop inc mx rs g -> do
  r <- readGauge g
  print r
  let rsx = if r > 3.0 then -r:rs else r:rs
  print rsx
  case doCheck inc rsx of
    Left   e  -> panic (show e)
    Right rs' ->
      if r < mx
        then loop inc mx rs' g
        else pure r
 where
  doCheck inc rs' = checkDecr 0.0 rs' >>= checkMaxFlow inc

x01al3Good :: IO Double
x01al3Good  = x01a 1.0 1.0 10.0 l3 -- detects negative flow

------------------------------------------------------------------------------

newtype PositiveFlowNT = PositiveFlowNT [Double]
newtype FlowOkNT       = FlowOkNT       [Double]

checkDecrNT :: [Double] -> Either GaugeException PositiveFlowNT
checkDecrNT xs =
  checkDecr 0.0 xs >>= pure . PositiveFlowNT

checkMaxFlowNT :: Double -> PositiveFlowNT -> Either GaugeException FlowOkNT
checkMaxFlowNT mx (PositiveFlowNT xs) =
  checkMaxFlow mx  xs >>= pure . FlowOkNT

lNT :: Double -> Double -> FlowOkNT -> Gauge Double -> IO Double
lNT  = fix $ \loop inc mx (FlowOkNT rs) g -> do
  r <- readGauge g
  print r
  let rsx = if r > 3.0 then -r:rs else r:rs
  print rsx
  case doCheck inc rsx of
    Left   e  -> panic (show e)
    Right rs' ->
      if r < mx
        then loop inc mx rs' g
        else pure r
 where
  doCheck :: Double -> [Double] -> Either GaugeException FlowOkNT
  doCheck inc rs' = checkDecrNT rs' >>= checkMaxFlowNT inc

x01b
  :: Double -> Double -> Double
  -> (Double -> Double -> FlowOkNT -> Gauge Double -> IO Double)
  -> IO Double
x01b incGauge incCheck mx lop =
  Gauge.new137 0.0 incGauge mx >>=
  lop incCheck mx (FlowOkNT [0.0])

x01blNTGood :: IO Double
x01blNTGood  = x01b 1.0 1.0 10.0 lNT -- detects negative flow

------------------------------------------------------------------------------

data PositiveFlow
data FlowOk
newtype GaugeReadingsPT p = GaugeReadingsPT [Double]

checkDecrPT :: [Double] -> Either GaugeException (GaugeReadingsPT PositiveFlow)
checkDecrPT xs =
  checkDecr 0.0 xs >>= pure . GaugeReadingsPT

checkMaxFlowPT
  :: Double -> GaugeReadingsPT PositiveFlow -> Either GaugeException (GaugeReadingsPT FlowOk)
checkMaxFlowPT mx (GaugeReadingsPT xs) =
  checkMaxFlow mx  xs >>= pure . GaugeReadingsPT

lPT :: Double -> Double -> GaugeReadingsPT FlowOk -> Gauge Double -> IO Double
lPT  = fix $ \loop inc mx (GaugeReadingsPT rs) g -> do
  r <- readGauge g
  print r
  let rsx = if r > 3.0 then -r:rs else r:rs
  print rsx
  case doCheck inc rsx of
    Left   e  -> panic (show e)
    Right rs' ->
      if r < mx
        then loop inc mx rs' g
        else pure r
 where
  doCheck :: Double -> [Double] -> Either GaugeException (GaugeReadingsPT FlowOk)
  doCheck inc rs' = checkDecrPT rs' >>= checkMaxFlowPT inc

x01c
  :: Double -> Double -> Double
  -> (Double -> Double -> GaugeReadingsPT FlowOk -> Gauge Double -> IO Double)
  -> IO Double
x01c incGauge incCheck mx lop =
  Gauge.new137 0.0 incGauge mx >>=
  lop incCheck mx (GaugeReadingsPT [0.0])

x01cl5NTGood :: IO Double
x01cl5NTGood  = x01c 1.0 1.0 10.0 lPT -- detects negative flow

------------------------------------------------------------------------------

type PositiveFlowOk = Refined (And PositiveFlow FlowOk) [Double]

instance Predicate PositiveFlowOk [Double] where
  validate p v = case checkDecr 0.0 v of
    Left e -> throwRefineSomeException (typeOf p) (toException e)
    Right v' -> case checkMaxFlow 1.0 v' of
      Left e -> throwRefineSomeException (typeOf p) (toException e)
      Right _ -> pure ()

lRT :: Double -> Refined PositiveFlowOk [Double] -> Gauge Double -> IO Double
lRT  = fix $ \loop mx rs g -> do
  r <- readGauge g
  print r
  let rsx = if r > 3.0 then -r:unrefine rs else r:unrefine rs
  print rsx
  case refine rsx of
    Left  e  ->
      if | isDecrException e        -> panic (show NotDecr)
         | isExceedsMaxFlowException e -> panic (show ExceedsMaxFlow)
         | otherwise                   -> panic (show e)
    Right rs' ->
      if r < mx
        then loop mx rs' g
        else pure r

x01d
  :: Double -> Double
  -> (Double -> Refined PositiveFlowOk [Double] -> Gauge Double -> IO Double)
  -> IO Double
x01d incGauge mx lop =
  case refine [] of
    Left  e  -> panic (show e)
    Right rs -> do
      g    <- Gauge.new137 0.0 incGauge mx
      lop mx rs g

x01dlRTGood :: IO Double
x01dlRTGood  = x01d 1.0 10.0 lRT -- detects negative flow

------------------------------------------------------------------------------

isDecrException :: RefineException -> Bool
isDecrException  = isGaugeException NotDecr

isExceedsMaxFlowException :: RefineException -> Bool
isExceedsMaxFlowException  = isGaugeException ExceedsMaxFlow

isGaugeException :: GaugeException -> RefineException -> Bool
isGaugeException ge = \case
  RefineSomeException _ e | fromException e == Just ge -> True
  _ -> False

