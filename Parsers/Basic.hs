module Parsers.Basic (module Parsers.Basic, module Base.ParsingBase) where

import Base.ParsingBase

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