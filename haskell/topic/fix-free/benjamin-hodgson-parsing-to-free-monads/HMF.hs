{-# OPTIONS_GHC -Wno-unused-do-bind                     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures            #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE TypeOperators             #-}

module HMF where

import           Control.Applicative
import           Control.Monad.Free
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  hiding (foldr1)
import           Data.Functor

data Ex a = forall i. Wrap (a i)

-- | runtime representation of type parameter
data Ty a where
  ListIntTy :: Ty [Int]
  UnitTy    :: Ty ()

-- | Pairs up two type constructors.
-- Ensures their parameters are equal.
-- Pattern match on Ty.
data (a :*: b) i = a i :&: b i

-- | Sig(ma) type (i.e., dependent pair)
-- type of second component depends on value of first
type    Sig a b = Ex   (a :*: b)
pattern Sig x y = Wrap (x :&: y)

data HmfCmdF a
 = FlushPage   [Int]    a
 | PageMisses ([Int] -> a)
 deriving Functor

type HmfCmd = Free HmfCmdF

fp :: [Int] -> HmfCmd ()
fp is = liftF $ FlushPage is ()

pm :: HmfCmd [Int]
pm = liftF $ PageMisses id

parseList :: Parser [Int]
parseList = do char '['; t <- decimal `sepBy` char ','; char ']'; return t

parseFP :: Parser (HmfCmd ())
parseFP = skipSpace *> string "flushPage" *> skipSpace *> fmap fp parseList

parsePM :: Parser (HmfCmd [Int])
parsePM = skipSpace *> string "pageMisses" $> pm

parseFPPM :: Parser (Sig Ty HmfCmd)
parseFPPM =  fmap (Sig UnitTy)    parseFP
         <|> fmap (Sig ListIntTy) parsePM

parseSeparator :: Parser ()
parseSeparator = skipSpace *> char ';' *> skipSpace

parseHmfParser :: Parser (Sig Ty HmfCmd)
parseHmfParser = fmap (foldr1 combine) $ parseFPPM `sepBy1` parseSeparator
 where combine (Sig _ val) (Sig ty acc) = Sig ty (val >> acc)
       combine          _            _  = error "parseHmf"

parseHmf :: ByteString -> Result (Sig Ty HmfCmd)
parseHmf = parse parseHmfParser

