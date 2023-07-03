module Parsers.Basic (module Parsers.Basic, module Base.ParsingBase) where

import Base.ParsingBase

whitespaceCharP :: Parser Char
whitespaceCharP = multiCharP [' ', '\t', '\n', '\r', '\v', '\f']

minAlphaCharP :: Parser Char
minAlphaCharP = multiCharP ['a' .. 'z']

capAlphaCharP :: Parser Char
capAlphaCharP = multiCharP ['A' .. 'Z']

alphaCharP :: Parser Char
alphaCharP = capAlphaCharP ||| minAlphaCharP

digitCharP :: Parser Char
digitCharP = multiCharP ['0' .. '9']

digitsP :: Parser String
digitsP = oblGreedify digitCharP

alphaNumCharP :: Parser Char
alphaNumCharP = alphaCharP ||| digitCharP

alphaNumStringP :: Parser String
alphaNumStringP = oblGreedify alphaNumCharP