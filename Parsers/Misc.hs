module Parsers.Misc (module Parsers.Misc, module Base.ParsingBase) where

import Base.ParsingBase
import Parsers.Basic
import Text.Read (readMaybe)

-- Some JSON-ish experiments
-- These are more or less just proof-of-concepts.
-- Many serious parsers would probably use plenty of monadic types, for certain a lot in JSON.

whitespaceP :: Parser String
whitespaceP = oblGreedify whitespaceCharP

intP :: Parser Int
intP = Parser f
  where
    f i = do
      (rem, parsed) <- runParser (charP '+' ?|> (charP '-' ?*++ digitsP)) i
      parsedAndRead <- readMaybe parsed :: Maybe Int
      Just (rem, parsedAndRead)

posIntP :: Parser Int
posIntP = charP '+' ?|> unsignedIntP 

unsignedIntP :: Parser Int
unsignedIntP = Parser f
  where
    f i = do
      (rem, parsed) <- runParser digitsP i
      parsedAndRead <- readMaybe parsed :: Maybe Int
      Just (rem, parsedAndRead)

floatP :: Parser Float
floatP = Parser f
  where
    f i = do
      (rem, parsed) <- runParser floatReadableP i
      parsedAndRead <- readMaybe parsed :: Maybe Float
      Just (rem, parsedAndRead)

    floatReadableP :: Parser String
    floatReadableP = charP '+' ?|> (charP '-' ?*++ (digitsP ?! "0") +++ (charP '.' *++ digitsP))

doubleQuotedStringLiteralP :: Parser String -- NOTICE: just takes in raw input string resembling a string.
doubleQuotedStringLiteralP =
  charP '"' *++ quotedStringContent '\"' ++* charP '"'

singleQuotedStringLiteralP :: Parser String -- NOTICE: just takes in raw input string resembling a string.
singleQuotedStringLiteralP =
  charP '\'' *++ quotedStringContent '\'' ++* charP '\''

quotedStringContent :: Char -> Parser String
quotedStringContent q = greedifyStr (matchStringP "\\\\" ||| (stringify (charP '\\') ++* notCharP '\\') ||| stringify (notMultiCharP [q, '\\']))

quotedStringLiteralP :: Parser String -- NOTICE: just takes in raw input string resembling a string.
quotedStringLiteralP = doubleQuotedStringLiteralP ||| singleQuotedStringLiteralP

singleQuotedStringP :: Parser String -- NOTICE: uses haskell's readMaybe to handle escapes after replacing the outer single quotes with double quotes
singleQuotedStringP = Parser f
  where
    f i = do
      (rem, parsed) <- runParser singleQuotedStringLiteralP i
      parsedAndRead <- readMaybe (doubleQuotify parsed) :: Maybe String
      Just (rem, parsedAndRead)

    doubleQuotify :: String -> String
    doubleQuotify (a : as) = '"' : escapeDqs (init as) ++ "\""

    escapeDqs "" = ""
    escapeDqs (a : as)
      | a == '"' = "\\\"" ++ escapeDqs as
      | otherwise = a : escapeDqs as

doubleQuotedStringP :: Parser String -- NOTICE: uses haskell's readMaybe to handle escapes
doubleQuotedStringP = Parser f
  where
    f i = do
      (rem, parsed) <- runParser doubleQuotedStringLiteralP i
      parsedAndRead <- readMaybe parsed :: Maybe String
      Just (rem, parsedAndRead)

stringP :: Parser String
stringP = doubleQuotedStringP ||| singleQuotedStringP

stringListP :: Parser [String]
stringListP = listP stringP

emptyListP :: Parser [a]
emptyListP = Parser f
  where
    f i = do
      (rem, parsed) <- runParser (stringify (charP '[') <|? whitespaceP ?|> stringify (charP ']')) i
      Just (rem, [])

listP :: Parser a -> Parser [a] -- NOTICE: Only supports same output type parsers in list
listP p = emptyListP ||| (charP '[' |> (greedify (listElemP p) ++* p <|? whitespaceP) <| charP ']')

listElemP :: Parser a -> Parser a -- NOTICE: A trailing comma is mandatory for this parser.
listElemP p = (whitespaceP ?|> p <|? whitespaceP) <| charP ',' <|? whitespaceP