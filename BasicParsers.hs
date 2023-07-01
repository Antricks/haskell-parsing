module BasicParsers (module BasicParsers, module ParsingBase) where

import ParsingBase

end :: Parser () -- fails if string is not empty, returns unit
end = Parser f
  where
    f "" = Just ("", ())
    f _ = Nothing

whitespaceCharParser :: Parser Char
whitespaceCharParser = multiCharP [' ', '\t', '\n', '\r', '\v', '\f']

minAlphaCharParser :: Parser Char
minAlphaCharParser = multiCharP ['a' .. 'z']

capAlphaCharParser :: Parser Char
capAlphaCharParser = multiCharP ['A' .. 'Z']

alphaCharParser :: Parser Char
alphaCharParser = capAlphaCharParser ||| minAlphaCharParser

digitCharParser :: Parser Char
digitCharParser = multiCharP ['0' .. '9']

digitsParser :: Parser String
digitsParser = oblGreedify digitCharParser

alphaNumCharParser :: Parser Char
alphaNumCharParser = alphaCharParser ||| digitCharParser