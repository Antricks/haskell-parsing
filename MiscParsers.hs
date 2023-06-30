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

doubleQuotedStringLiteralParser :: Parser String -- NOTICE: just takes in raw input string resembling a string. Does not handle quote escapes.
doubleQuotedStringLiteralParser = charP '"' *++ greedify (notCharP '"') ++* charP '"'

singleQuotedStringLiteralParser :: Parser String -- NOTICE: just takes in raw input string resembling a string. Does not handle quote escapes.
singleQuotedStringLiteralParser = charP '\'' *++ greedify (notCharP '\'') ++* charP '\''

quotedStringLiteralParser :: Parser String -- NOTICE: just takes in raw input string resembling a string. Does not handle quote escapes.
quotedStringLiteralParser = doubleQuotedStringLiteralParser ||| singleQuotedStringLiteralParser

singleQuotedStringParser :: Parser String -- NOTICE: uses haskell's read to handle escapes after replacing the single quotes with double quotes
singleQuotedStringParser = Parser f -- TODO FIXME: crashes when a double quote is included in the single quoted string
  where
    f i = case runParser singleQuotedStringLiteralParser i of
      Just (rem, parsed@(p : _)) -> Just (rem, read (doubleQuotify parsed) :: String) -- TODO: readMaybe
      Nothing -> Nothing
      where
        doubleQuotify :: String -> String
        doubleQuotify (a : as) = '"' : init as ++ "\""

doubleQuotedStringParser :: Parser String -- NOTICE: uses haskell's read to handle escapes
doubleQuotedStringParser = Parser f
  where
    f i = case runParser doubleQuotedStringLiteralParser i of
      Just (rem, parsed@(p : _)) -> Just (rem, read parsed :: String) -- TODO: readMaybe
      Nothing -> Nothing

stringParser :: Parser String
stringParser = doubleQuotedStringParser ||| singleQuotedStringParser

stringListParser :: Parser [String]
stringListParser = listParser stringParser

listParser :: Parser a -> Parser [a]
listParser p = charP '[' |> (greedify (listElemParser p) ++* p) <| wsParser <| charP ']'

listElemParser :: Parser a -> Parser a
listElemParser p = (wsParser ?|> p <|? wsParser) <| charP ',' <|? wsParser