module Parser where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.Text

type IniDocument = HashMap Text (HashMap Text Text)

parseIniDocument :: Text -> Either String IniDocument
parseIniDocument = parseOnly (document <* (endOfInput <|> pure ()))

whiteSpace :: Parser ()
whiteSpace =
  many1 space *> whiteSpace
  <|> pure ()

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

sectionName :: Parser Text
sectionName = lexeme $ do
  _ <- char '['
  name <- takeTill (== ']')
  _ <- char ']'
  pure $ toLower name

field :: Parser (Text, Text)
field = lexeme $ do
  key <- takeWhile1 (\c -> c /= '=' && c /= '[')
  _ <- char '='
  value <- takeTill isEndOfLine
  pure (toLower key, value)

section :: Parser (Text, HashMap Text Text)
section = lexeme $ do
  name <- sectionName
  body <- HM.fromList <$> many field
  pure (name, body)

document :: Parser IniDocument
document = do
  whiteSpace
  HM.fromList <$> many section
