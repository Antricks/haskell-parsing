module Base.CharSets where

whitespaceCS :: [Char]
whitespaceCS = [' ', '\t', '\n', '\r', '\v', '\f']

digitCS :: [Char]
digitCS = ['0' .. '9']

majAlphaCS :: [Char]
majAlphaCS = ['A' .. 'Z']

minAlphaCS :: [Char]
minAlphaCS = ['a' .. 'z']

alphaCS :: [Char]
alphaCS = minAlphaCS ++ majAlphaCS

alphaNumCS :: [Char]
alphaNumCS = alphaCS ++ digitCS