{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module X_16_example
where
{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 15:15:38 2003>
   License:    GPL

Created       : 2015 Aug 31 (Mon) 07:19:45 by Harold Carr.
Last Modified : 2015 Aug 31 (Mon) 11:25:18 by Harold Carr.
-}

{-
Example 16 - Using the Reader monad
-}

import           "mtl" Control.Monad.Reader

import           Control.Arrow                       (second)
-- import           Data.List                           (intercalate)
import           Data.Maybe
import           System.Environment                  (getArgs)
import           Text.ParserCombinators.Parsec
-- import           Text.ParserCombinators.Parsec.Token

--                   Text       Variable     Quote        Include                   Compound
data Template      = T String | V Template | Q Template | I Template [Definition] | C [Template]
                   deriving Show
data Definition    = D Template Template
                   deriving Show
data NamedTemplate = NT String Template
                   deriving Show
{-
instance Show Template where
  show (T s)    = s
  show (V t)    = "${"  ++ show t ++ "}"
  show (Q t)    = "$\"" ++ show t ++ "\""
  show (I t ds) = let name        = show t
                      definitions = intercalate ", " (map show ds)
                  in case definitions of
		       []        -> "$<" ++ name ++ ">"
		       otherwise -> "$<" ++ name ++ "|" ++ definitions ++ ">"
  show (C ts)   = concatMap show ts

instance Show Definition where
  show (D t d) = show t ++ "=" ++ show d

instance Show NamedTemplate where
  show (NT n t) = "[" ++ n ++ "]" ++ show t ++ "[END]\n"
-}
-- parse a file containing named templates
templateFile :: Parser [NamedTemplate]
templateFile = do nts <- many namedTemplate
                  eof
                  return nts

-- parse a single named template
namedTemplate :: Parser NamedTemplate
namedTemplate = do
  n <- name
  t <- template [] <?> "template"
  _ <- end
  spaces
  return (NT n t)

-- parse a named template label
name :: Parser String
name = between (char '[') (char ']') (many1 (noneOf "]")) <?> "label"

-- parse a named template [END] keyword
end :: Parser String
end = string "[END]" <?> "[END]"

-- parse a (possibly compound) template.
-- the [Char] argument is a list of characters not allowed in the template.
template :: String -> Parser Template
template except = do ts <- many1 (simpleTemplate except)
                     case ts of
                       [t] -> return t
                       _   -> return (C ts)

-- parse a simple template: text, a variable pattern, a quote pattern, or a include pattern
-- the [Char] argument is a list of characters not allowed in the template.
simpleTemplate :: String -> Parser Template
simpleTemplate except =  text except
                     <|> try variable
                     <|> try quote
                     <|> include

-- parse a dollar-sign that doesn't begin a variable, quote, or include pattern
dollar :: Parser Char
dollar = try (do c <- char '$'
                 notFollowedBy (oneOf "{<\"")
                 return c)
         <?> ""

-- parse a left bracket that isn't part of an [END] keyword
leftBracket :: Parser Char
leftBracket = try (do s <- try end <|> string "["
                      case s of
                        "[END]" -> pzero
                        "["     -> return '['
                        _       -> error "leftBracket")
              <?> ""

-- parse a character that isn't part of a pattern or END keyword and
-- isn't in the list of excluded characters.
textChar :: String -> Parser Char
textChar except = noneOf ("$[" ++ except) <|> dollar <|> leftBracket

-- parse a string of allowed characters
-- the [Char] argument is a list of characters not allowed in the text.
text :: String -> Parser Template
text except = do str <- many1 (textChar except)
                 return (T str)
              <?> "text"

-- parse a variable pattern
variable :: Parser Template
variable = do t <- between (string "${") (char '}') (template "}")
              return (V t)
           <?> "variable pattern"

-- parse a quoted-inclusion pattern
quote :: Parser Template
quote = do t <- between (string "$\"") (char '\"') (template "\"")
           return (Q t)
           <?> "quoted include pattern"

-- parse a resolved-inclusion pattern
include :: Parser Template
include = between (string "$<") (char '>') includeBody
          <?> "include pattern"

-- parse the body of an inclusion pattern
includeBody :: Parser Template
includeBody = do t  <- template "|>"
                 ds <- option [] definitions
                 return (I t ds)

-- parse a list of definitions
definitions :: Parser [Definition]
definitions = do
  _ <- char '|'
  definition `sepBy1` char ','

-- parse a single definition
definition :: Parser Definition
definition = do
  t1 <- template "=,>"
  _ <- char '='
  t2 <- template ",>"
  return (D t1 t2)
  <?> "variable definition"

-- Environment: named templated and named var assoc lists.
data Environment = Env {templates::[(String,Template)],
                        variables::[(String,String)]}

lookupVar :: String -> Environment -> Maybe String
lookupVar name0 env = lookup name0 (variables env)

lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate name0 env = lookup name0 (templates env)

addDefs :: [(String,String)] -> Environment -> Environment
addDefs defs env = env {variables = defs ++ variables env}

resolveDef :: Definition -> Reader Environment (String,String)
resolveDef (D t d) = do
  name0 <- resolve t
  value <- resolve d
  return (name0,value)

resolve :: Template -> Reader Environment String
resolve (T s)    = return s
resolve (V t)    = do varName  <- resolve t
                      varValue <- asks (lookupVar varName)
                      return $ fromMaybe "" varValue
resolve (Q t)    = do tmplName <- resolve t
                      body     <- asks (lookupTemplate tmplName)
                      return $ maybe "" show body
resolve (I t ds) = do tmplName <- resolve t
                      body     <- asks (lookupTemplate tmplName)
                      case body of
                        Just t' -> do defs <- mapM resolveDef ds
                                      local (addDefs defs) (resolve t')
                        Nothing -> return ""
resolve (C ts)     = fmap concat (mapM resolve ts)

-- turn a named template into a (name,template) pair
stripName :: NamedTemplate -> (String, Template)
stripName (NT n t) = (n,t)

{-
1st arg is filename  (e.g., template.txt) that contains templates in [name]...[END] pairs.
Templates are text. The special sequences ${var}, $"name", and $<name> cause substitutions.
${var} is replaced by value of the var.
$"name" is replaced by the named template.
- The template is "quoted", so var patterns, etc. in it are not treated specially.
$<name> inserts the named template and performs all substitutions in that template.
Var values introduced/overriden in an included template using
   $<name|var1=value1,var2=value2,...,varN=valueN>

2nd arg is an initial template to evaluate.
It references named templates in the templates file.

Remaining args are var bindings for the initial template : "var=value"
-}

main :: IO ()
main = do args     <- getArgs
          -- read CLI args
          let tmplFile = head args
          let pat      = args!!1
          let defs     = drop 2 args
          main2 tmplFile pat defs

main2 :: String -> String -> [String] -> IO ()
main2 tmplFile patt defs = do
          -- parse template file
          nts      <- parseFromFile templateFile tmplFile
          case nts of
            (Left err) -> print err
            (Right _)  -> return ()
          -- parse user template
          let tmpl = parse (template []) "pattern" patt
          case tmpl of
            (Left err) -> print err
            (Right _)  -> return ()
          -- parse var defs
          let ds     = map (break (=='=')) defs
              ds'    = map (Control.Arrow.second tail) ds
          let ntl    = either (const []) id nts
              -- construct env
              env    = Env (map stripName ntl) ds'
              t      = either (const (T "")) id tmpl
              -- do the work
              result = runReader (resolve t) env
          putStr result

test :: IO ()
test = do
    let t = "templates.txt"
    main2 t "$<#1>"                 [""]
    main2 t "${language}"           ["language=Haskell"]
    main2 t "$\"#3\""               [""]
    main2 t "$<#3>"                 [""]
    main2 t "===$<no such file>===" [""]
    main2 t "$<#2>"                 [""]
    main2 t "$<#2>"                 ["var=dog"]
    main2 t "$<#2|var=dog>"         [""]
    main2 t "$<#4|variable=cat>"    [""]
    main2 t "$<#5>"                 ["which=3"]
    main2 t "$<#5|which=3>"         [""]
    main2 t "$<#6|which=5>"         [""]
    main2 t "$<#6|which=5,var=dog,variable=cat>" [""]

-- END OF FILE
