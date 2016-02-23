{-# LANGUAGE TypeFamilies, FlexibleConMultiParamTypeClasses, RankNTypes,
    GAUndecidableInstances, NoMonomorphismRestriction, TypeOperators,  LambdaCase, ConstraintKinds #-}

module HCCrossImpl where

import Prelude hiding (mapM, mapM_, sequence, sequence_, foldl, and)

import Reflex.Class
import Reflex.Dynamic
import qualified HCPure as P

import Control.Monad.Identity hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Arrow ((&&&))
import Data.Traversable
import Data.Foldable
import Control.Monad.Writer hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import System.Exit

type PureReflexDomain = P.Pure Int
type TimeM = (->) Int

type TestCaseConstraint t m = (Reflex t, MonadSample t m, MonadHold t m, MonadFix m, MonadFix (PushM t))

data TestCase = forall a b c d. (Eq c, Eq d, Show a, Show b, Show c, Show d)
                => TestCase (Map Int a, Map Int b)
                            (forall m t. TestCaseConstraint t m
                             => (Behavior t a, Event t b) -> m (Behavior t c, Event t d))

mapToPureBehavior :: Map Int a -> Behavior PureReflexDomain a
mapToPureBehavior m = P.Behavior $ \t -> case Map.lookupLE t m of
  Nothing -> error $ "mapToPureBehavior: no value for time " <> show t
  Just (_, v) -> v

mapToPureEvent :: Map Int a -> Event PureReflexDomain a
mapToPureEvent m = P.Event $ flip Map.lookup m

relevantTestingTimes :: (Map Int a, Map Int b) -> [Int]
relevantTestingTimes (b, e) = case Set.minView &&& Set.maxView $ Map.keysSet b `Set.union` Map.keysSet e of
  (Just (t0, _), Just (t1, _)) -> [t0..t1+1] -- We need to go to b+1 to see the result of the final event
  _                            -> [] -- Doesn't actually make much sense

testPure :: (t ~ PureReflexDomain, m ~ TimeM) => ((Behavior t a, Event t b) -> m (Behavior t c, Event t d)) -> (Map Int a, Map Int b) -> (Map Int c, Map Int d)
testPure builder (b, e) =
  let (P.Behavior b', P.Event e') = ($ 0) $ builder (mapToPureBehavior b, mapToPureEvent e)
      relevantTimes = relevantTestingTimes (b, e)
      e'' = Map.mapMaybe id $ Map.fromList $ map (id &&& e') relevantTimes
      b'' = Map.fromList $ map (id &&& b') relevantTimes
  in (b'', e'')

testAgreement :: (Eq c, Eq d, Show a, Show b, Show c, Show d) => (forall m t. TestCaseConstraint t m => (Behavior t a, Event t b) -> m (Behavior t c, Event t d)) -> (Map Int a, Map Int b) -> IO Bool
testAgreement builder inputs@(inputsB, inputsE) = do
    let identityResult = testPure builder inputs
    let (b,e) = (Map.toList inputsB, Map.toList inputsE)
    putStr "i: "
    print (show (b,e))
    putStr "r: "
    print identityResult
    return True

test :: IO ()
test = do
    results <- forM testCases $ \(name, TestCase inputs builder) -> do
        putStrLn "--------------"
        putStrLn name
        testAgreement builder inputs
    exitWith $ if and results
               then ExitSuccess
               else ExitFailure 1

testCases :: [(String, TestCase)]
testCases =
  [ (,) "hold" $ TestCase (Map.singleton 0 "bHold0", Map.fromList [(1, "eHold1"), (2, "eHold2")]) $ \(_, e) -> do
       b' <- hold "holdInit" e
       return (b', e)
  , (,) "count" $ TestCase (Map.singleton 0 (), Map.fromList [(1, ()), (2, ()), (3, ())]) $ \(_, e) -> do
       e' <- liftM updated $ count e
       b' <- hold (0 :: Int) e'
       return (b', e')
  , (,) "onceE-1" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       e' <- onceE $ leftmost [e, e]
       return (b, e')
  , (,) "switch-1" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       b' <- hold never e'
       let e'' = switch b'
       return (b, e'')
  , (,) "switch-2" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ const $ do
             let ea = fmap (const "a") e
             let eb = fmap (const "b") e
             let eab = leftmost [ea, eb]
             liftM switch $ hold eab never
           e'' = coincidence e'
       return (b, e'')
  , (,) "switch-3" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ const $ do
             let ea = fmap (const "a") e
             let eb = fmap (const "b") e
             let eab = leftmost [ea, eb]
             liftM switch $ hold eab (fmap (const e) e)
           e'' = coincidence e'
       return (b, e'')
  , (,) "switch-4" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- liftM switch $ hold e' (fmap (const e) e)
       return (b, e'')
  , (,) "switchPromptly-1" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       e'' <- switchPromptly never e'
       return (b, e'')
  , (,) "switchPromptly-2" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       e'' <- switchPromptly never $ leftmost [e', e']
       return (b, e'')
  , (,) "switchPromptly-3" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switchPromptly never (fmap (const e) e')
       return (b, e'')
  , (,) "switchPromptly-4" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj"), (3, "asdf")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switchPromptly never (fmap (const e') e)
       return (b, e'')
  , (,) "switch-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- liftM switch $ hold never (fmap (const e') e)
       return (b, e'')
  , (,) "switchPromptly-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip push e $ \_ -> do
             return . Just =<< onceE e
       e'' <- switchPromptly never e'
       return (b, e'')
  , (,) "switchPromptly-6" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> do
             switchPromptly e never
       e'' <- switchPromptly never e'
       return (b, e'')
  , (,) "coincidence-1" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> return $ fmap id e
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-2" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> return $ leftmost [e, e]
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-3" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> return $ coincidence $ fmap (const e) e
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-4" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj"), (3, "asdf")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> onceE e
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer")]) $ \(b, e) -> do
       let eChild = flip pushAlways e $ const $ do
             let eNewValues = leftmost [e, e]
             return $ coincidence $ fmap (const eNewValues) eNewValues
           e' = coincidence eChild
       return (b, e')
  , (,) "coincidence-6" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer")]) $ \(b, e) -> do
       let eChild = flip pushAlways e $ const $ do
             let e' = coincidence $ fmap (const e) e
             return $ leftmost [e', e']
           e'' = coincidence eChild
       return (b, e'')
  , (,) "coincidence-7" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj"), (3, "asdf")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
           eCoincidences = coincidence $ fmap (const e') e
       return (b, eCoincidences)
  , (,) "holdWhileFiring" $ TestCase (Map.singleton 0 "zxc", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       eo <- onceE e
       bb <- hold b $ pushAlways (const $ hold "asdf" eo) eo
       let b' = pull $ sample =<< sample bb
       return (b', e)
  , (,) "joinDyn" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       bb <- hold "b" e
       bd <- hold never . fmap (const e) =<< onceE e
       eOuter <- liftM (pushAlways sample . fmap (const bb)) $ onceE e
       let eInner = switch bd
           e' = leftmost [eOuter, eInner]
       return (b, e')
  ]

