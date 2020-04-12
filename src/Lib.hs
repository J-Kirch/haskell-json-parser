module Lib where

import Data.Char
import Control.Applicative

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser 
  { runParser :: String -> Maybe (String, a) 
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> 
    case p input of
      Nothing -> Nothing
      Just (xs, a) -> Just (xs, f a)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =  Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> 
    p1 input <|> p2 input

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> stringLiteral <* charP '"')

stringLiteral :: Parser String
stringLiteral = spanP (/= '"')

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNullP (spanP isDigit)
  where f ds = JsonNumber $ read ds

notNullP :: Parser [a] -> Parser [a]
notNullP (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then Nothing
    else Just (input', xs)

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where f "true" = JsonBool True
        f "false" = JsonBool False
        f _ = error "no parse"

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> 
  let (token, rest) = span f input
  in Just (rest, token)

charP :: Char -> Parser Char
charP x = Parser f
  where f (y:ys) 
          | x == y = Just (ys, x)
          | otherwise = Nothing
        f _ = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP


someFunc :: IO ()
someFunc = putStrLn "someFunc"
