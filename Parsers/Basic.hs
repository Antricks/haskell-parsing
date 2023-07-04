module Parsers.Basic (module Parsers.Basic, module Base.ParsingBase) where

import Base.CharSets
import Base.ParsingBase

whitespaceCharP :: Parser Char
whitespaceCharP = multiCharP whitespaceCS

minAlphaCharP :: Parser Char
minAlphaCharP = multiCharP minAlphaCS

majAlphaCharP :: Parser Char
majAlphaCharP = multiCharP majAlphaCS

alphaCharP :: Parser Char
alphaCharP = multiCharP alphaCS

digitCharP :: Parser Char
digitCharP = multiCharP digitCS

alphaNumCharP :: Parser Char
alphaNumCharP = multiCharP alphaNumCS

digitsP :: Parser String
digitsP = oblGreedify digitCharP

alphaNumStringP :: Parser String
alphaNumStringP = oblGreedify alphaNumCharP