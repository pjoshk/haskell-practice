-- TODO: allow escaping of qoutes
-- TODO: support floating point numbers

import Control.Applicative
import Data.Char

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Integer
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

-- INTERFACES --
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (a, input') <- p input
    Just (f a, input')

instance Applicative Parser where
  pure a = Parser (\input -> Just (a, input))
  (Parser p) <*> (Parser a) = Parser $ \input -> do
    (f, input') <- p input
    (a, input'') <- a input'
    Just (f a, input'')

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser a <|> Parser b = Parser $ \input -> a input <|> b input

-- HELPER FUNCTIONS --
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (token, input') <- p input
  if null token
    then Nothing
      else Just (token, input')

split :: Parser a -> Parser b -> Parser [b]
split seperator element = (:) <$> element <*> many (seperator *> element) <|> pure []

spaces :: Parser String
spaces = parseWithPredicate isSpace

parseChar :: Char -> Parser Char
parseChar c = Parser f
  where
    f (y : ys)
      | y == c = Just (y, ys)
      | otherwise = Nothing
    f [] = Nothing

parseStr :: String -> Parser String
parseStr = traverse parseChar


stringLiteral :: Parser String
stringLiteral = parseChar '"' *> parseWithPredicate (/= '"') <* parseChar '"'

parseWithPredicate :: (Char -> Bool) -> Parser String
parseWithPredicate f = Parser $ \input -> do
  let (token, input') = span f input
   in Just (token, input')

-- PARSER IMPLEMENTATIONS --
parseJson :: Parser JsonValue
parseJson = parseNull <|> parseBool <|> parseNumber <|> parseString <|> parseArray <|> parseObject

parseNull :: Parser JsonValue
parseNull = JsonNull <$ parseStr "null"

parseTrue :: Parser JsonValue
parseTrue = JsonBool True <$ parseStr "true"

parseFalse :: Parser JsonValue
parseFalse = JsonBool False <$ parseStr "false"

parseBool :: Parser JsonValue
parseBool = parseTrue <|> parseFalse

parseNumber :: Parser JsonValue
parseNumber = JsonNumber . read <$> notNull (parseWithPredicate isDigit)

parseString :: Parser JsonValue
parseString = JsonString <$> stringLiteral

parseArray :: Parser JsonValue
parseArray =  parseChar '[' *> spaces *> elements <* spaces <* parseChar ']'
  where elements = JsonArray <$> split (spaces *> parseChar ',' <* spaces) parseJson

parseObject :: Parser JsonValue
parseObject = JsonObject <$> (parseChar '{' *> spaces *> split (spaces *> parseChar ',' <* spaces) kvPairs <* spaces <* parseChar '}')
  where kvPairs = (\k _ v -> (k, v)) <$> stringLiteral <*> (spaces *> parseChar ':' <* spaces) <*> parseJson

