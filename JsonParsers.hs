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
jsonKeyValueParser = Parser f
  where
    f i = case runParser ((jsonKeyObjParser <|? wsParser) <| charP ':' <|? wsParser) i of
      Just (rem_key, parsed_key) -> case runParser (jsonObjParser <|? wsParser) rem_key of
        Just (rem_val, parsed_val) -> Just (rem_val, (parsed_key, parsed_val))
        Nothing -> Nothing
      Nothing -> Nothing

jsonDictParser :: Parser JsonObj
jsonDictParser = Parser f
  where
    dictRawParser :: Parser [(JsonObj, JsonObj)]
    dictRawParser = charP '{' |> (wsParser ?|> (greedify (jsonKeyValueParser <| charP ',' <|? wsParser) ++* jsonKeyValueParser) <|? wsParser) <| charP '}'

    f i = case runParser dictRawParser i of
      Just (rem, parsed) -> Just (rem, JsonDict parsed)
      Nothing -> Nothing

jsonObjParser :: Parser JsonObj
jsonObjParser = jsonFloatParser ||| jsonIntParser ||| jsonStringParser ||| jsonListParser

jsonKeyObjParser :: Parser JsonObj
jsonKeyObjParser = jsonFloatParser ||| jsonIntParser ||| jsonStringParser