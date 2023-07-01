module JsonParsers (module JsonParsers, module ParsingBase) where

import BasicParsers
import MiscParsers
import ParsingBase

data JsonObj = JsonInt Int | JsonFloat Float | JsonString String | JsonList [JsonObj] | JsonDict [(JsonObj, JsonObj)] deriving (Show, Eq)

-- I can use quite a few of parsers from misc here, I just have to wrap their output into a shared data type

jsonIntParser :: Parser JsonObj
jsonIntParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser intParser i
      return (rem, JsonInt parsed)

jsonFloatParser :: Parser JsonObj
jsonFloatParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser floatParser i
      return (rem, JsonFloat parsed)

jsonStringParser :: Parser JsonObj
jsonStringParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser stringParser i
      return (rem, JsonString parsed)

jsonListParser :: Parser JsonObj
jsonListParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser (listParser jsonObjParser) i
      return (rem, JsonList parsed)

jsonKeyValueParser :: Parser (JsonObj, JsonObj)
jsonKeyValueParser = Parser f
  where
    f i = do
      (rem_key, parsed_key) <- runParser ((jsonKeyObjParser <|? wsParser) <| charP ':' <|? wsParser) i
      (rem_val, parsed_val) <- runParser (jsonObjParser <|? wsParser) rem_key
      return (rem_val, (parsed_key, parsed_val))

jsonDictParser :: Parser JsonObj
jsonDictParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser dictRawParser i
      return (rem, JsonDict parsed)

    dictRawParser :: Parser [(JsonObj, JsonObj)]
    dictRawParser = emptyDictParser ||| (charP '{' |> (wsParser ?|> (greedify (jsonKeyValueParser <| charP ',' <|? wsParser) ++* jsonKeyValueParser) <|? wsParser) <| charP '}')

    emptyDictParser :: Parser [a] -- code duplication from emptyListParser in misc
    emptyDictParser = Parser f
      where
        f i = do
          (rem, parsed) <- runParser (stringify (charP '{') <|? wsParser ?|> stringify (charP '}')) i
          return (rem, [])

jsonObjParser :: Parser JsonObj
jsonObjParser = jsonFloatParser ||| jsonIntParser ||| jsonStringParser ||| jsonListParser ||| jsonDictParser

jsonKeyObjParser :: Parser JsonObj
jsonKeyObjParser = jsonFloatParser ||| jsonIntParser ||| jsonStringParser