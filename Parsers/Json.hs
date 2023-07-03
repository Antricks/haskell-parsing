module Parsers.Json (module Parsers.Json, module Base.ParsingBase) where

import Base.ParsingBase
import Parsers.Basic
import Parsers.Misc

data JsonObj = JsonInt Int | JsonFloat Float | JsonString String | JsonList [JsonObj] | JsonDict [(JsonObj, JsonObj)] deriving (Show, Eq)

-- I can use quite a few of parsers from misc here, I just have to wrap their output into a shared data type

jsonIntP :: Parser JsonObj
jsonIntP = wrap JsonInt intP

jsonFloatP :: Parser JsonObj
jsonFloatP = wrap JsonFloat floatP

jsonStringP :: Parser JsonObj
jsonStringP = wrap JsonString stringP

jsonListP :: Parser JsonObj
jsonListP = wrap JsonList $ listP jsonObjP

jsonKeyValueP :: Parser (JsonObj, JsonObj)
jsonKeyValueP = Parser f
  where
    f i = do
      (rem_key, parsed_key) <- runParser ((jsonKeyObjP <|? whitespaceP) <| charP ':' <|? whitespaceP) i
      (rem_val, parsed_val) <- runParser (jsonObjP <|? whitespaceP) rem_key
      Just (rem_val, (parsed_key, parsed_val))

jsonDictP :: Parser JsonObj
jsonDictP = Parser f
  where
    f i = do
      (rem, parsed) <- runParser dictRawP i
      Just (rem, JsonDict parsed)

    dictRawP :: Parser [(JsonObj, JsonObj)]
    dictRawP = emptyDictP ||| (charP '{' |> (whitespaceP ?|> (greedify (jsonKeyValueP <| charP ',' <|? whitespaceP) ++* jsonKeyValueP) <|? whitespaceP) <| charP '}')

    emptyDictP :: Parser [a] -- some code duplication from emptyListP in misc
    emptyDictP = Parser f
      where
        f i = do
          (rem, parsed) <- runParser (stringify (charP '{') <|? whitespaceP ?|> stringify (charP '}')) i
          Just (rem, [])

jsonObjP :: Parser JsonObj
jsonObjP = jsonFloatP ||| jsonIntP ||| jsonStringP ||| jsonListP ||| jsonDictP

jsonKeyObjP :: Parser JsonObj
jsonKeyObjP = jsonFloatP ||| jsonIntP ||| jsonStringP