module BaseParse where

import Data.Maybe
import Data.Text.Internal.Builder.Int.Digits (digits)

newtype Parser t = Parser {runParser :: String -> Maybe (String, t)}

-------------------------
-- PARSER CONSTRUCTORS --
-------------------------

charP :: Char -> Parser Char -- constructs a parser accepting one char that matches the given char
charP a = Parser f
  where
    f "" = Nothing
    f (y : ys)
      | y == a = Just (ys, y)
      | otherwise = Nothing

notCharP :: Char -> Parser Char -- constructs a parser accepting one char that does NOT match the given char
notCharP a = Parser f
  where
    f "" = Nothing
    f (y : ys)
      | y /= a = Just (ys, y)
      | otherwise = Nothing

multiCharP :: [Char] -> Parser Char -- constructs a parser accepting one char if it is an element of the given char list
multiCharP chars = Parser f
  where
    f "" = Nothing
    f (y : ys)
      | y `elem` chars = Just (ys, y)
      | otherwise = Nothing

stringP :: String -> Parser String -- constructs a parser accepting strictly the given string
stringP "" = undefined
stringP (a : "") = stringify (charP a)
stringP (a : as) = stringify (charP a) +++ stringP as

----------------------------------
-- unitary operators on parsers --
----------------------------------

stringify :: Parser Char -> Parser String
stringify p = Parser f
  where
    f input = case runParser p input of
      out@(Just (rem, parsed)) -> Just (rem, [parsed])
      Nothing -> Nothing

greedify :: Parser Char -> Parser String -- NOTICE: will also return the empty string. Creates a string parser that takes in a string until the given char parser fails.
greedify p = Parser f
  where
    f = f' ""
      where
        f' :: String -> String -> Maybe (String, String)
        f' buf remains
          | isNothing nextParse = Just (remains, reverse buf) -- reverse so we can append the characters at the head of the buf string -> should improve performance for big strings
          | otherwise =
              let Just (newRemains, parsedChar) = nextParse in f' (parsedChar : buf) newRemains
          where
            nextParse = runParser p remains

obligatoryStr :: Parser String -> Parser String -- makes a string parser fail when returning the empty string
obligatoryStr p = Parser f
  where
    f input = case runParser p input of
      out@(Just (rem, parsed)) ->
        case parsed of
          "" -> Nothing
          _ -> out
      Nothing -> Nothing

oblGreedify :: Parser Char -> Parser String
oblGreedify = obligatoryStr . greedify

repeatStr :: Integral a => a -> Parser String -> Parser String -- chains together the same string parser n times
repeatStr 1 p = p
repeatStr n p
  | n <= 0 = undefined -- these cases do not make any sense, undefined is just right here
  | otherwise = p +++ repeatStr (n - 1) p

---------------------------
-- Verkettungsoperatoren --
---------------------------

(|||) :: Parser a -> Parser a -> Parser a -- if parser a returns Nothing, try parser b
(|||) pa pb = Parser f
  where
    f input
      | isJust $ runParser pa input = runParser pa input
      | otherwise = runParser pb input

(|>) :: Parser a -> Parser b -> Parser b -- returns parsing output only from parser b but a result from parser a is obligatory 
pa |> pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) ->
        case runParser pb rem_a of
          Just (rem_b, parsed_b) -> Just (rem_b, parsed_b)
          Nothing -> Nothing
      Nothing -> Nothing

(<|) :: Parser a -> Parser b -> Parser a -- returns parsing output only from parser a but a result from parser b is obligatory
pa <| pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) ->
        case runParser pb rem_a of
          Just (rem_b, parsed_b) -> Just (rem_b, parsed_a)
          Nothing -> Nothing
      Nothing -> Nothing

(?|>) :: Parser a -> Parser b -> Parser b -- returns parsing output only from parser b, a result from parser a is not obligatory. If parser a fails, parser b just works on the initial input
pa ?|> pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, _) -> runParser pb rem_a
      Nothing -> runParser pb i

(<|?) :: Parser a -> Parser b -> Parser a -- returns parsing output only from parser a, a result from parser b is not obligatory. If parser b fails, the remains from parser a's output are returned
pa <|? pb = Parser f
  where
    f i = case runParser pa i of
      out_a@(Just (rem_a, parsed_a)) ->
        case runParser pb rem_a of
          Just (rem_b, _) -> Just (rem_b, parsed_a)
          Nothing -> out_a
      Nothing -> Nothing

(+++) :: Parser String -> Parser String -> Parser String -- chains together two string parsers and concatenates their parsed outputs
pa +++ pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) ->
        case runParser pb rem_a of
          Just (rem_b, parsed_b) -> Just (rem_b, parsed_a ++ parsed_b)
          Nothing -> Nothing
      Nothing -> Nothing

(*++) :: Parser Char -> Parser String -> Parser String -- chains together a char parser and a string parser and concatenates their parsed outputs
pa *++ pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) ->
        case runParser pb rem_a of
          Just (rem_b, parsed_b) -> Just (rem_b, parsed_a : parsed_b)
          Nothing -> Nothing
      Nothing -> Nothing

(++*) :: Parser String -> Parser Char -> Parser String -- chains together a string parser and a char parser and concatenates their parsed outputs
pa ++* pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) ->
        case runParser pb rem_a of
          Just (rem_b, parsed_b) -> Just (rem_b, parsed_a ++ [parsed_b])
          Nothing -> Nothing
      Nothing -> Nothing

--------------------
-- actual parsers --
--------------------

end :: Parser () -- fails if string is not empty, returns unit
end = Parser f
  where
    f "" = Just ("", ())
    f _ = Nothing

whitespaceCharParser :: Parser Char
whitespaceCharParser = multiCharP [' ', '\t', '\n', '\r', '\v', '\f']

wsParser :: Parser String
wsParser = oblGreedify whitespaceCharParser

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

intParser :: Parser Int
intParser = Parser f
  where
    f i = case runParser digitsParser i of
      Just (rem, parsed) -> Just (rem, read parsed :: Int)
      Nothing -> Nothing

floatParser :: Parser Float
floatParser = Parser f
  where
    prepareParser :: Parser String
    prepareParser = digitsParser ++* charP '.' +++ digitsParser

    f i = case runParser prepareParser i of
      Just (rem, parsed) -> Just (rem, read parsed :: Float)
      Nothing -> Nothing

alphaNumCharParser :: Parser Char
alphaNumCharParser = alphaCharParser ||| digitCharParser

doubleQuotedStringParser :: Parser String
doubleQuotedStringParser = charP '"' *++ greedify (notCharP '"') ++* charP '"' -- NOTICE: does not work with escapes yet.

singleQuotedStringParser :: Parser String
singleQuotedStringParser = charP '\'' *++ greedify (notCharP '\'') ++* charP '\'' -- NOTICE: does not work with escapes yet.

quotedStringParser :: Parser String
quotedStringParser = doubleQuotedStringParser ||| singleQuotedStringParser
