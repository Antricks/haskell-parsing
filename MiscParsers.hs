module MiscParsers where

import BasicParsers
import ParsingBase
import Text.Read (readMaybe)

-- Some JSON-ish experiments
-- These are more or less just proof-of-concepts.
-- Many serious parsers would probably use plenty of monadic types, for certain a lot in JSON.

wsParser :: Parser String
wsParser = oblGreedify whitespaceCharParser

intParser :: Parser Int
intParser = Parser f
  where
    f i = case runParser digitsParser i of
      Just (rem, parsed) ->
        case readMaybe parsed :: Maybe Int of
          Just parsedAndRead -> Just (rem, parsedAndRead)
          Nothing -> Nothing
      Nothing -> Nothing

floatParser :: Parser Float
floatParser = Parser f
  where
    prepareParser :: Parser String
    prepareParser = digitsParser ++* charP '.' +++ digitsParser

    f i = case runParser prepareParser i of
      Just (rem, parsed) ->
        case readMaybe parsed :: Maybe Float of
          Just parsedAndRead -> Just (rem, parsedAndRead)
          Nothing -> Nothing
      Nothing -> Nothing

-- TODO take care of quote escapes
doubleQuotedStringLiteralParser :: Parser String -- NOTICE: just takes in raw input string resembling a string. Does not handle quote escapes.
doubleQuotedStringLiteralParser = charP '"' *++ greedify (notCharP '"') ++* charP '"'

-- TODO take care of quote escapes
singleQuotedStringLiteralParser :: Parser String -- NOTICE: just takes in raw input string resembling a string. Does not handle quote escapes.
singleQuotedStringLiteralParser = charP '\'' *++ greedify (notCharP '\'') ++* charP '\''

quotedStringLiteralParser :: Parser String -- NOTICE: just takes in raw input string resembling a string. Does not handle quote escapes.
quotedStringLiteralParser = doubleQuotedStringLiteralParser ||| singleQuotedStringLiteralParser

singleQuotedStringParser :: Parser String -- NOTICE: uses haskell's readMaybe to handle escapes after replacing the single quotes with double quotes
singleQuotedStringParser = Parser f -- TODO FIXME: fails when a double quote is included in the single quoted string
  where
    doubleQuotify :: String -> String
    doubleQuotify (a : as) = '"' : init as ++ "\"" -- TODO related: escape double quotes here
    f i = case runParser singleQuotedStringLiteralParser i of
      Just (rem, parsed) ->
        case read (doubleQuotify parsed) :: Maybe String of
          Just parsedAndRead -> Just (rem, parsedAndRead)
          Nothing -> Nothing
      Nothing -> Nothing

doubleQuotedStringParser :: Parser String -- NOTICE: uses haskell's readMaybe to handle escapes
doubleQuotedStringParser = Parser f
  where
    f i = case runParser doubleQuotedStringLiteralParser i of
      Just (rem, parsed) ->
        case readMaybe parsed :: Maybe String of
          Just parsedAndRead -> Just (rem, parsedAndRead)
          Nothing -> Nothing
      Nothing -> Nothing

stringParser :: Parser String
stringParser = doubleQuotedStringParser ||| singleQuotedStringParser

stringListParser :: Parser [String]
stringListParser = listParser stringParser

emptyListParser :: Parser [a]
emptyListParser = Parser f
  where
    f i = case runParser (stringify (charP '[') <|? wsParser ?|> stringify (charP ']')) i of
      Just (rem, parsed) -> Just (rem, [])
      Nothing -> Nothing

listParser :: Parser a -> Parser [a] -- NOTICE: Only supports same-type parsers in list, would need monadic types otherwise
listParser p = emptyListParser ||| (charP '[' |> (greedify (listElemParser p) ++* p <|? wsParser) <| charP ']')

listElemParser :: Parser a -> Parser a -- NOTICE: A trailing comma is mandatory for this parser.
listElemParser p = (wsParser ?|> p <|? wsParser) <| charP ',' <|? wsParser