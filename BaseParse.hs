module BaseParse where

import Data.Maybe
import Data.Text.Internal.Builder.Int.Digits (digits)

newtype Parser t = Parser {runParser :: String -> Maybe (String, t)}

--------------------------
-- Parser Konstruktoren --
--------------------------

charP :: Char -> Parser Char -- Baut einen Parser, der einen bestimmten Char entgegen nimmt
charP a = Parser f
  where
    f "" = Nothing
    f (y : ys)
      | y == a = Just (ys, y)
      | otherwise = Nothing

notCharP :: Char -> Parser Char
notCharP a = Parser f
  where
    f "" = Nothing
    f (y : ys)
      | y /= a = Just (ys, y)
      | otherwise = Nothing

multiCharP :: [Char] -> Parser Char -- Baut einen Parser, der einen Char entgegennimmt, welcher aber aus einer vorgegebenen Liste stammen darf
multiCharP chars = Parser f
  where
    f "" = Nothing
    f (y : ys)
      | y `elem` chars = Just (ys, y)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP "" = undefined
stringP (a : "") = stringify (charP a)
stringP (a : as) = stringify (charP a) +++ stringP as

------------------------
-- Unitäre Operatoren --
------------------------

stringify :: Parser Char -> Parser String
stringify p = Parser f
  where
    f input = case runParser p input of
      out@(Just (rem, parsed)) -> Just (rem, [parsed])
      Nothing -> Nothing

greedify :: Parser Char -> Parser String -- Baut einen Parser, der einen möglichst langen String (akzeptiert auch den leeren) einliest, bis der nächste Char nicht mehr vom Char Parser angenommen wird
greedify p = Parser f
  where
    f = f' ""
      where
        f' :: String -> String -> Maybe (String, String)
        f' buf remains
          | isNothing nextParse = Just (remains, reverse buf) -- reverse um chars vorne anhängen zu können -> performance
          | otherwise =
              let Just (newRemains, parsedChar) = nextParse in f' (parsedChar : buf) newRemains
          where
            nextParse = runParser p remains

obligatoryStr :: Parser String -> Parser String
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

repeatStr :: Integral a => a -> Parser String -> Parser String
repeatStr 1 p = p
repeatStr n p
  | n <= 0 = undefined -- Diesen Fall gibt es nicht, ein explizites undefined ist hier also völlig richtig.
  | otherwise = p +++ repeatStr (n - 1) p

---------------------------
-- Verkettungsoperatoren --
---------------------------

parserOr :: Parser a -> Parser a -> Parser a -- Baut einen Parser, der den ersten Parser und bei Miserfolg den zweiten versucht
parserOr pa pb = Parser f
  where
    f input
      | isJust $ runParser pa input = runParser pa input
      | otherwise = runParser pb input

(|||) = parserOr -- Zucker für das parserOr

(|>) :: Parser a -> Parser b -> Parser b -- Kettet einen String Parser hinter einen obligatorischen Char Parser, dessen Parse-Output ignoriert wird
pa |> pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) ->
        case runParser pb rem_a of
          Just (rem_b, parsed_b) -> Just (rem_b, parsed_b)
          Nothing -> Nothing
      Nothing -> Nothing

(<|) :: Parser a -> Parser b -> Parser a -- Kettet einen String Parser vor einen obligatorischen Char Parser, dessen Parse-Output ignoriert wird
pa <| pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) ->
        case runParser pb rem_a of
          Just (rem_b, parsed_b) -> Just (rem_b, parsed_a)
          Nothing -> Nothing
      Nothing -> Nothing

(?|>) :: Parser a -> Parser b -> Parser b
pa ?|> pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, _) -> runParser pb rem_a
      Nothing -> runParser pb i

(<|?) :: Parser a -> Parser b -> Parser a
pa <|? pb = Parser f
  where
    f i = case runParser pa i of
      out_a@(Just (rem_a, parsed_a)) ->
        case runParser pb rem_a of
          Just (rem_b, _) -> Just (rem_b, parsed_a)
          Nothing -> out_a
      Nothing -> Nothing

(+++) :: Parser String -> Parser String -> Parser String -- Kettet einen String Parser hinter einen obligatorischen Char Parser, dessen Parse-Output mitgenommen wird
pa +++ pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) ->
        case runParser pb rem_a of
          Just (rem_b, parsed_b) -> Just (rem_b, parsed_a ++ parsed_b)
          Nothing -> Nothing
      Nothing -> Nothing

(*++) :: Parser Char -> Parser String -> Parser String -- Kettet einen String Parser hinter einen obligatorischen Char Parser, dessen Parse-Output mitgenommen wird
pa *++ pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) ->
        case runParser pb rem_a of
          Just (rem_b, parsed_b) -> Just (rem_b, parsed_a : parsed_b)
          Nothing -> Nothing
      Nothing -> Nothing

(++*) :: Parser String -> Parser Char -> Parser String -- Kettet einen String Parser vor einen obligatorischen Char Parser, dessen Parse-Output mitgenommen wird
pa ++* pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) ->
        case runParser pb rem_a of
          Just (rem_b, parsed_b) -> Just (rem_b, parsed_a ++ [parsed_b])
          Nothing -> Nothing
      Nothing -> Nothing

---------------------
-- konkrete Parser --
---------------------

end :: Parser ()
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
      Just (rem, parsed) -> Just (rem, read parsed::Int)
      Nothing -> Nothing

floatParser :: Parser Float
floatParser = Parser f
  where
    prepareParser :: Parser String
    prepareParser = digitsParser ++* charP '.' +++ digitsParser
    
    f i = case runParser prepareParser i of
      Just (rem, parsed) -> Just (rem, read parsed::Float)
      Nothing -> Nothing
    
alphaNumCharParser :: Parser Char
alphaNumCharParser = alphaCharParser ||| digitCharParser

doubleQuotedStringParser :: Parser String
doubleQuotedStringParser = charP '"' *++ greedify (notCharP '"') ++* charP '"'

singleQuotedStringParser :: Parser String
singleQuotedStringParser = charP '\'' *++ greedify (notCharP '\'') ++* charP '\''

quotedStringParser :: Parser String
quotedStringParser = doubleQuotedStringParser ||| singleQuotedStringParser
