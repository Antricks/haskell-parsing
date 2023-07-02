module Parsers.Misc (module Parsers.Misc, module Base.ParsingBase) where

import Base.ParsingBase
import Parsers.Basic
import Text.Read (readMaybe)

-- Some JSON-ish experiments
-- These are more or less just proof-of-concepts.
-- Many serious parsers would probably use plenty of monadic types, for certain a lot in JSON.

wsParser :: Parser String
wsParser = oblGreedify whitespaceCharParser

intParser :: Parser Int
intParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser (charP '+' ?|> (charP '-' *?++ digitsParser)) i
      parsedAndRead <- readMaybe parsed :: Maybe Int
      Just (rem, parsedAndRead)

floatParser :: Parser Float
floatParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser floatReadableParser i
      parsedAndRead <- readMaybe parsed :: Maybe Float
      Just (rem, parsedAndRead)

    floatReadableParser :: Parser String
    floatReadableParser = charP '+' ?|> (charP '-' *?++ (digitsParser ?! "0") +++ (charP '.' *++ digitsParser))

doubleQuotedStringLiteralParser :: Parser String -- NOTICE: just takes in raw input string resembling a string.
doubleQuotedStringLiteralParser = 
  charP '"' *++ quotedStringContent '\"' ++* charP '"'

singleQuotedStringLiteralParser :: Parser String -- NOTICE: just takes in raw input string resembling a string.
singleQuotedStringLiteralParser =
  charP '\'' *++ quotedStringContent '\'' ++* charP '\''

quotedStringContent :: Char -> Parser String
quotedStringContent q = greedifyStr (stringP "\\\\" ||| (stringify (charP '\\') ++* notCharP '\\') ||| stringify (notMultiCharP [q, '\\']))

quotedStringLiteralParser :: Parser String -- NOTICE: just takes in raw input string resembling a string.
quotedStringLiteralParser = doubleQuotedStringLiteralParser ||| singleQuotedStringLiteralParser

singleQuotedStringParser :: Parser String -- NOTICE: uses haskell's readMaybe to handle escapes after replacing the outer single quotes with double quotes
singleQuotedStringParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser singleQuotedStringLiteralParser i
      parsedAndRead <- readMaybe (doubleQuotify parsed) :: Maybe String
      Just (rem, parsedAndRead)

    doubleQuotify :: String -> String
    doubleQuotify (a : as) = '"' : escapeDqs (init as) ++ "\""

    escapeDqs "" = ""
    escapeDqs (a : as)
      | a == '"' = "\\\"" ++ escapeDqs as
      | otherwise = a : escapeDqs as

doubleQuotedStringParser :: Parser String -- NOTICE: uses haskell's readMaybe to handle escapes
doubleQuotedStringParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser doubleQuotedStringLiteralParser i
      parsedAndRead <- readMaybe parsed :: Maybe String
      Just (rem, parsedAndRead)

stringParser :: Parser String
stringParser = doubleQuotedStringParser ||| singleQuotedStringParser

stringListParser :: Parser [String]
stringListParser = listParser stringParser

emptyListParser :: Parser [a]
emptyListParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser (stringify (charP '[') <|? wsParser ?|> stringify (charP ']')) i
      Just (rem, [])

listParser :: Parser a -> Parser [a] -- NOTICE: Only supports same output type parsers in list
listParser p = emptyListParser ||| (charP '[' |> (greedify (listElemParser p) ++* p <|? wsParser) <| charP ']')

listElemParser :: Parser a -> Parser a -- NOTICE: A trailing comma is mandatory for this parser.
listElemParser p = (wsParser ?|> p <|? wsParser) <| charP ',' <|? wsParser