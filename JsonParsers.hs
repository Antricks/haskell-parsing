module JsonParsers where

import BasicParsers
import MiscParsers
import ParsingBase

data JsonObj = JsonInt Int | JsonFloat Float | JsonString String | JsonList [JsonObj] | JsonDict [(JsonObj, JsonObj)] deriving (Show, Eq)

-- I can use quite a few of parsers from misc here, I just have to wrap their output into a monadic type

jsonIntParser :: Parser JsonObj
jsonIntParser = Parser f
  where
    f i = case runParser intParser i of
      Just (rem, parsed) -> Just (rem, JsonInt parsed)
      Nothing -> Nothing

jsonFloatParser :: Parser JsonObj
jsonFloatParser = Parser f
  where
    f i = case runParser floatParser i of
      Just (rem, parsed) -> Just (rem, JsonFloat parsed)
      Nothing -> Nothing

jsonStringParser :: Parser JsonObj
jsonStringParser = Parser f
  where
    f i = case runParser stringParser i of
      Just (rem, parsed) -> Just (rem, JsonString parsed)
      Nothing -> Nothing

jsonListParser :: Parser JsonObj
jsonListParser = Parser f
  where
    f i = case runParser (listParser jsonObjParser) i of
      Just (rem, parsed) -> Just (rem, JsonList parsed)
      Nothing -> Nothing

jsonKeyValueParser :: Parser (JsonObj, JsonObj)
jsonKeyValueParser = undefined -- TODO implement - That's going to be a bit trickier...

jsonDictParser :: Parser [(JsonObj, JsonObj)]
jsonDictParser = charP '{' |> (wsParser ?|> greedify jsonKeyValueParser <|? wsParser) <| charP '}' -- I could theoretically abstract this to misc and then wrap it like the others...

jsonObjParser :: Parser JsonObj
jsonObjParser = jsonFloatParser ||| jsonIntParser ||| jsonStringParser ||| jsonListParser

jsonKeyParser :: Parser JsonObj
jsonKeyParser = jsonFloatParser ||| jsonIntParser ||| jsonStringParser