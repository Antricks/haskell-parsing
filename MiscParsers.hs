module MiscParsers where

import Base
import BaseParsers

-- Some JSON-ish experiments

intParser :: Parser Int
intParser = Parser f
  where
    f i = case runParser digitsParser i of
      Just (rem, parsed) -> Just (rem, read parsed :: Int) -- TODO: readMaybe
      Nothing -> Nothing

floatParser :: Parser Float
floatParser = Parser f
  where
    prepareParser :: Parser String
    prepareParser = digitsParser ++* charP '.' +++ digitsParser

    f i = case runParser prepareParser i of
      Just (rem, parsed) -> Just (rem, read parsed :: Float) -- TODO: readMaybe
      Nothing -> Nothing

doubleQuotedStringParser :: Parser String -- NOTICE: just takes in raw input string resembling a string. Does not handle quote escapes.
doubleQuotedStringParser = charP '"' *++ greedify (notCharP '"') ++* charP '"'

singleQuotedStringParser :: Parser String -- NOTICE: just takes in raw input string resembling a string. Does not handle quote escapes.
singleQuotedStringParser = charP '\'' *++ greedify (notCharP '\'') ++* charP '\''

quotedStringParser :: Parser String -- NOTICE: just takes in raw input string resembling a string. Does not handle quote escapes.
quotedStringParser = doubleQuotedStringParser ||| singleQuotedStringParser 

stringLiteralParser :: Parser String -- NOTICE: uses haskell's read to handle escapes
stringLiteralParser = Parser f
  where
    f i = case runParser doubleQuotedStringParser i of -- NOTICE: single quotes are not supported for string literals in Haskell and not read as such.
      Just (rem, parsed@(p:_)) -> Just (rem, read parsed::String) -- TODO: readMaybe
      Nothing -> Nothing

stringListParser :: Parser [String]
stringListParser = charP '[' |> greedify (wsParser ?|> stringLiteralParser <|? wsParser <|? charP ',' <|? wsParser) <| charP ']'