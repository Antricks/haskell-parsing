module Parsers.Json (module Parsers.Json, module Base.ParsingBase) where

import Base.ParsingBase
import Parsers.Basic
import Parsers.Misc

data JsonObj = JsonInt Int | JsonFloat Float | JsonString String | JsonList [JsonObj] | JsonDict [(JsonObj, JsonObj)] deriving (Show, Eq)

-- I can use quite a few of parsers from misc here, I just have to wrap their output into a shared data type

jsonIntParser :: Parser JsonObj
jsonIntParser = wrap JsonInt intParser

jsonFloatParser :: Parser JsonObj
jsonFloatParser = wrap JsonFloat floatParser

jsonStringParser :: Parser JsonObj
jsonStringParser = wrap JsonString stringParser

jsonListParser :: Parser JsonObj
jsonListParser = wrap JsonList $ listParser jsonObjParser

jsonKeyValueParser :: Parser (JsonObj, JsonObj)
jsonKeyValueParser = Parser f
  where
    f i = do
      (rem_key, parsed_key) <- runParser ((jsonKeyObjParser <|? wsParser) <| charP ':' <|? wsParser) i
      (rem_val, parsed_val) <- runParser (jsonObjParser <|? wsParser) rem_key
      Just (rem_val, (parsed_key, parsed_val))

jsonDictParser :: Parser JsonObj
jsonDictParser = Parser f
  where
    f i = do
      (rem, parsed) <- runParser dictRawParser i
      Just (rem, JsonDict parsed)

    dictRawParser :: Parser [(JsonObj, JsonObj)]
    dictRawParser = emptyDictParser ||| (charP '{' |> (wsParser ?|> (greedify (jsonKeyValueParser <| charP ',' <|? wsParser) ++* jsonKeyValueParser) <|? wsParser) <| charP '}')

    emptyDictParser :: Parser [a] -- code duplication from emptyListParser in misc
    emptyDictParser = Parser f
      where
        f i = do
          (rem, parsed) <- runParser (stringify (charP '{') <|? wsParser ?|> stringify (charP '}')) i
          Just (rem, [])

jsonObjParser :: Parser JsonObj
jsonObjParser = jsonFloatParser ||| jsonIntParser ||| jsonStringParser ||| jsonListParser ||| jsonDictParser

jsonKeyObjParser :: Parser JsonObj
jsonKeyObjParser = jsonFloatParser ||| jsonIntParser ||| jsonStringParser