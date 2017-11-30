{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module PA where

import           Control.Applicative
import           Control.Monad                    (void)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as BSC
import           GHC.Generics
import           GHC.Int                          (Int64)

------------------------------------------------------------------------------

-- * Internal constants
reservedWords :: [String]
reservedWords = [ "assert"
                , "is"
                , "append"
                , "prepend"
                , "send"
                , "wait"
                , "timed"
                , "load"
                , "debuginfo"
                ]

-- * Utility Parsing Functions

parseProgram :: String -> Result (Program String)
parseProgram = run (ignore *> pProgramStrLit)

run p = parse (ignore *> p) . BSC.pack

-- Not used
-- test :: (Show a) => Parser a -> String -> IO ()
-- test p = parseTest (ignore *> p) . BSC.pack

lexeme :: Parser a -> Parser a
lexeme p = p <* ignore

ignore :: Parser ()
ignore = skipSpace >> (skipComment <|> return ())
  where
    skipComment
      = try (string "--")
      >> manyTill anyChar endOfLine
      >> (ignore <|> return ())

oneOf :: String -> Parser Char
oneOf str = satisfy (`elem` str)

symbol :: BSC.ByteString -> Parser ()
symbol s = void (lexeme (string s)) <?> BSC.unpack s
-- MSM: The <?> notation allows us to "name the parser, in case failure occurs", according to:
-- https://hackage.haskell.org/package/attoparsec-0.13.1.0/docs/Data-Attoparsec-ByteString-Char8.html
-- However, we are not using this in any meaningful way.  So we just see a generic message when
-- parsing fails, which only identifies the last parser: endOfInput.

ident :: Parser String
ident = lexeme (ensureNoSymb rawident) <?> "Identifier"
  where
    rawident = (:) <$> letter_ascii
                   <*> many (letter_ascii
                        <|> digit
                        <|> oneOf "_'")

    isReserved :: String -> Bool
    isReserved = (`elem` reservedWords)

    ensureNoSymb p
      = do res <- p
           if isReserved res
           then fail $ "Reserved word: " ++ res
           else return res

comma :: Parser ()
comma = symbol ","

between :: Parser a -> Parser b -> Parser a -> Parser b
between pre p pos = try pre *> p <* pos

braces :: Parser a -> Parser a
braces p = between (symbol "{") p (symbol "}")

stringLiteral :: Parser String
stringLiteral
  =   between (symbol "\"") (go '"')  (symbol "\"")
  <|> between (symbol "'")  (go '\'') (symbol "'")
  where
    go delim
      = do c <- peekChar
           if c == Just delim
           then return ""
           else if c == Just '\\'
                then (:) <$> (anyChar >> choice (escapeSeqs delim)) <*> go delim
                else (:) <$> anyChar <*> go delim

    escapeSeqs delim
      = [ char 'n'   >> return '\n'
        , char 't'   >> return '\t'
        , char 'r'   >> return '\r'
        , char '\\'  >> return '\\'
        , char delim >> return delim
        ]

-- * Parsing the AST

pShardId :: Parser ShardId
pShardId = lexeme decimal <?> "ShardId"

pShardRange :: Parser ShardRange
pShardRange
  =   try (braces (SRSet <$> (pShardId `sepBy1` comma) <?> "ShardId Set"))
  <|> (braces (SRRange <$> (pShardId <* symbol "..") <*> pShardId) <?> "ShardId range")
  <|> (SRSingle <$> pShardId)

pOptShardRange :: Parser (Maybe ShardRange)
pOptShardRange = option Nothing (Just <$> pShardRange)

pNameRange :: Parser [String]
pNameRange
  =   braces (ident `sepBy1` comma)
  <|> ((:[]) <$> ident)

pShardedLocalCmd :: Parser a -> Parser (ShardedCmd a)
pShardedLocalCmd pInner
  = do cmd   <- BSC.unpack <$> pLocalCmd
       case lookup cmd (localCmdParsers pInner) of
         Nothing -> fail $ "Unknown command: " ++ cmd
         Just p  -> ShardedCmd <$> (symbol "#" *> pShardRange)
                               <*> p
  where
    optionalInt :: Parser Int
    optionalInt = lexeme (fromIntegral <$> option (1 :: Int) decimal)
                <?> "Optional replication factor"

    pLocalCmd :: Parser BSC.ByteString
    pLocalCmd
      = (choice . map (string . BSC.pack . fst) $ localCmdParsers (return ()))
      <?> "Local Command"

    localCmdParsers :: Parser a -> [ (String , Parser (LocalCmd a)) ]
    localCmdParsers pInner'
      = [ ("append"  , LCmdAppend  <$> optionalInt <*> pInner' )
        , ("prepend" , LCmdPrepend <$> optionalInt <*> pInner' )
        ]

pNamed :: Parser a -> Parser (Named a)
pNamed p
  = Named <$> option Nothing (Just <$> (ident <* symbol "<-"))
          <*> p

pInstruction :: Parser a -> Parser (Instruction a)
pInstruction pInner = choice
  [ (InstNamedCmd <$> pNamed (pShardedLocalCmd pInner))         <?> "Named Cmd"
  , (InstWait     <$> (symbol "wait" *> option [] pNameRange))  <?> "Wait"
  , (InstSend     <$> (symbol "send" *> pOptShardRange))        <?> "Send"
  , (InstAssert   <$> (symbol "assert" *> ident <* symbol "is")
                  <*> braces (stringLiteral `sepBy1` comma))    <?> "Assert"
  , (const InstDebug <$> symbol "debuginfo")                    <?> "Debug Info"
  ] <?> "Instruction"


pProgram :: Parser a -> Parser (Program a)
pProgram pInner
  =   (ProgTimed <$> (symbol "timed" *> option "" stringLiteral)
                 <*> braces (pProgram pInner) <* many (symbol ";")
                 <*> pProgram pInner
  <|> (ProgSeq   <$> (pInstruction pInner <* many1 (symbol ";"))
                 <*> pProgram pInner)
  <|> return ProgEmpty) <?> "Program"

pProgramStrLit :: Parser (Program String)
pProgramStrLit = pProgram stringLiteral <* endOfInput

-----------------------------------------------
-- * Legacy Wrapper
--
-- Provides a convenient interface for choosing between loading a program from a file, providing a
-- script to run directly, or invoking a custom built-in script, such as the legacy "many test".
data CommandWrapper
  = ManyTest [Int]
  | Load String
  | Run (Program String)
  deriving (Eq , Show)

pCommandWrapper :: Parser CommandWrapper
pCommandWrapper = wrapper <* endOfInput
  where
    wrapper
      =   (ManyTest <$> (mapM_ symbol ["many" , "test" , ":"]
                    >> many1 (lexeme decimal)))
      <|> (Load     <$> (symbol "load" >> stringLiteral))
      <|> (Run      <$> pProgram stringLiteral)

parseCommandWrapper :: String -> Result CommandWrapper
parseCommandWrapper = run pCommandWrapper

------------------------------------------------------------------------------

type ShardId = Int64
data ShardRange
  -- ^ A single shard ids
  = SRSingle ShardId
  -- ^ A set of shard ids, written as @{1 , 4 , 5}@
  | SRSet [ShardId]
  -- ^ A range of shard ids, written as @{1 .. 5}@
  | SRRange ShardId ShardId
  deriving (Eq , Show , Generic)
data ShardedCmd a
  = ShardedCmd ShardRange (LocalCmd a)
  deriving (Eq , Show , Generic , Functor)
data LocalCmd a
  -- ^ Appends a command to the end of the queue n times
  = LCmdAppend Int a
  -- ^ Prepends a command to the beginning of the queue n times
  | LCmdPrepend Int a
  deriving (Eq , Show , Functor)
data Named cmd = Named
  { name  :: Maybe String
  , thing :: cmd
  } deriving (Eq , Show , Generic , Functor)
data Instruction a
  -- ^ We can change a shard-specific queue using a local command
  = InstNamedCmd (Named (ShardedCmd a))
  -- ^ Waits for some request id's before continue to process.
  | InstWait  [String]
  -- ^ Send the already flushed commands to the servers. Will also
  --   register RequestIds if any names were given.
  | InstSend (Maybe ShardRange)
  -- ^ Asserts the result stored for the given request id
  --   has the given results.
  | InstAssert String [String]
  -- ^ Prints all sort of debugging information
  | InstDebug
  deriving (Eq , Show , Generic , Functor)
data Program a
  = ProgSeq   (Instruction a) (Program a)
  | ProgTimed String (Program a) (Program a)
  | ProgEmpty
  deriving (Eq , Show , Generic , Functor)

------------------------------------------------------------------------------

data JcsParseResult r
  = PRFail { unconsumed :: BSC.ByteString
           , contexts   :: [String]
           , errMsg     :: String
           }
  | PRDone { unconsumed :: BSC.ByteString
           , result     :: r
           }
  deriving Show

parseJcsE :: String -> Program String
parseJcsE x = case parseJcs x of
  (PRDone _ r)   -> r
  fail           -> error $ show fail

parseJcs :: String -> JcsParseResult (Program String)
parseJcs x = case handlePartial x of
  Fail u ctxs msg -> PRFail u ctxs msg
  Done u r        -> PRDone u r
 where
  handlePartial x = case parseProgram x of
    Partial f  -> f ""
    failOrDone -> failOrDone

-- Some simple programs:

manytest :: String
manytest = unlines
  [ "prepend#{0,1} 1000 \"CreateOrUpdate Acct1\" ;"
  , "v <- append#{0,1} \"See\" ; -- llalala"
  , "timed 'sendblock' { send {0 , 1}; wait v; }"
  , "assert  is { 'res1' , 'res2' };"
  ]

-- and the parser stress-test

jcsAssert :: String
jcsAssert = unlines
  [ "-- Generates the famous 'many test: 100 100' we"
  , "-- have been using for a while."
  , "--"
  , "-- The main difference is that we are now generating"
  , "-- commands for the alternate command handler,"
  , "-- but the testing logic is the same."
  , "--"
  , ""
  , "-- First, we append 100 to be setn to shard 1"
  , "append#0 100 \"CreateOrUpdate Acct0\";"
  , "-- We also append a \"See\" command, that is going to "
  , "-- return the balance of account 1."
  , "-- We name the RequestId w0, so we can wait on it later;"
  , "w0 <- append#0 \"See\";"
  , ""
  , "-- Same for shard 1; on a different account just so"
  , "-- we are sure the results are different"
  , "append#1 100 \"CreateOrUpdate Acct1\";"
  , "w1 <- append#1 \"See\";"
  , ""
  , "-- We now start a clock to record how long does it take"
  , "-- to send the commands and wait for the result"
  , "-- of the \"see\"s"
  , "timed 'many test' {"
  , "  send {0 , 1};"
  , "  wait { w0 , w1 };"
  , "}"
  , ""
  , "-- Now we assert that the variables have the"
  , "-- correct value"
  , ""
  , "assert w0 is {"
  , "  \"\\nAccount: Amount\\n----------------------\\nAcct0: 99.0\\n\""
  , "};"
  , "assert w1 is {"
  , "  \"\\nAccount: Amount\\n----------------------\\nAcct1: 99.0\\n\""
  , "};"
  ]
