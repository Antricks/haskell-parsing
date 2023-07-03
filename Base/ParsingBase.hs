module Base.ParsingBase where

import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.Char

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

caseInsCharP :: Char -> Parser Char -- constructs a parser accepting one char that matches the given char
caseInsCharP a = Parser f
  where
    f "" = Nothing
    f (y : ys)
      | Data.Char.toUpper y == Data.Char.toUpper a = Just (ys, y)
      | otherwise = Nothing

notCharP :: Char -> Parser Char -- constructs a parser accepting one char that does NOT match the given char
notCharP a = Parser f
  where
    f "" = Nothing
    f (y : ys)
      | y /= a = Just (ys, y)
      | otherwise = Nothing

notMultiCharP :: [Char] -> Parser Char
notMultiCharP chars = Parser f
  where
    f "" = Nothing
    f (y : ys)
      | y `notElem` chars = Just (ys, y)
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
stringP (a : as) = charP a *++ stringP as

caseInsStringP :: String -> Parser String
caseInsStringP "" = undefined
caseInsStringP (a: "") = stringify (caseInsCharP a)
caseInsStringP (a: as) = caseInsCharP a *++ stringP as

anyCharP :: Parser Char -- Just accepts one char unconditionally
anyCharP = Parser f
  where
    f "" = Nothing
    f (a : as) = Just (as, a)

predP :: (a -> Bool) -> Parser a -> Parser a -- takes a predicate that is obligatory for the parser to succeed
predP pred parser = Parser f
  where
    f i = do
      out@(rem, parsed) <- runParser parser i

      if pred parsed
        then return out
        else Nothing

end :: Parser () -- fails if string is not empty, returns unit
end = Parser f
  where
    f "" = Just ("", ())
    f _ = Nothing

----------------------------------
-- UNITARY OPERATORS ON PARSERS --
----------------------------------

stringify :: Parser a -> Parser [a]
stringify p = Parser f
  where
    f i = do
      (rem, parsed) <- runParser p i
      Just (rem, [parsed])

greedify :: Parser a -> Parser [a] -- NOTICE: will also return the empty list. Creates a list parser that takes in until the given parser fails. Collects outputs in a list. This also applies to Char -> [Char]::String
greedify p = Parser f
  where
    f = f' []
      where
        f' buf remains
          | isNothing nextParse = Just (remains, reverse buf) -- reverse so we can append the characters at the head of the buf string -> should improve performance for big strings
          | otherwise =
              let Just (newRemains, parsedChar) = nextParse in f' (parsedChar : buf) newRemains
          where
            nextParse = runParser p remains

greedifyStr :: Parser [a] -> Parser [a] -- NOTICE: will also return the empty string. Creates a string parser that takes in a string until the given string parser fails. Concatenates outputs
greedifyStr p = Parser f
  where
    f = f' []
      where
        f' buf remains
          | isNothing nextParse = Just (remains, buf) -- reverse so we can append the characters at the head of the buf string -> should improve performance for big strings
          | otherwise =
              let Just (newRemains, parsedChar) = nextParse in f' (buf ++ parsedChar) newRemains
          where
            nextParse = runParser p remains

obligatoryListContent :: Parser [a] -> Parser [a] -- makes a parser fail when returning an empty list / the empty string
obligatoryListContent p = Parser f
  where
    f input = do
      out@(rem, parsed) <- runParser p input
      case parsed of
        [] -> Nothing
        _ -> Just out

oblGreedify :: Parser a -> Parser [a]
oblGreedify = obligatoryListContent . greedify

oblGreedifyStr :: Parser String -> Parser String
oblGreedifyStr = obligatoryListContent . greedifyStr

repeatStr :: Integral a => a -> Parser String -> Parser String -- chains together the same string parser n times
repeatStr 1 p = p
repeatStr n p
  | n <= 0 = undefined -- these cases do not make any sense, undefined is just right here
  | otherwise = p +++ repeatStr (n - 1) p

------------------------
-- PARSER COMBINATORS --
------------------------

(|||) :: Parser a -> Parser a -> Parser a -- if parser a returns Nothing, try parser b
(|||) pa pb = Parser f
  where
    f i
      | isJust out_a = out_a
      | otherwise = runParser pb i
      where
        out_a = runParser pa i

(?!) :: Parser a -> a -> Parser a -- returns a default value `s` if the parser `p` fails
p ?! a = Parser f
  where
    f i = case runParser p i of
      out@(Just (rem, parsed)) -> out
      Nothing -> Just (i, a)

(?+) :: Parser a -> b -> Parser b -- returns a default value `s` if the parser `p` succeeds
p ?+ a = Parser f
  where
    f i = do
      (rem, parsed) <- runParser p i
      Just (rem, a)

(|>) :: Parser a -> Parser b -> Parser b -- returns parsing output only from parser b but a result from parser a is obligatory
pa |> pb = Parser f
  where
    f i = do
      (rem_a, _) <- runParser pa i
      (rem_b, parsed_b) <- runParser pb rem_a
      Just (rem_b, parsed_b)

(<|) :: Parser a -> Parser b -> Parser a -- returns parsing output only from parser a but a result from parser b is obligatory
pa <| pb = Parser f
  where
    f i = do
      (rem_a, parsed_a) <- runParser pa i
      (rem_b, parsed_b) <- runParser pb rem_a
      Just (rem_b, parsed_a)

(?|>) :: Parser a -> Parser b -> Parser b -- returns parsing output only from parser b, a result from parser a is not obligatory. If parser a fails, parser b just works on the initial input
pa ?|> pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, _) -> runParser pb rem_a
      Nothing -> runParser pb i

(<|?) :: Parser a -> Parser b -> Parser a -- returns parsing output only from parser a, a result from parser b is not obligatory. If parser b fails, the remains from parser a's output are returned
pa <|? pb = Parser f
  where
    f i = do
      out_a@(rem_a, parsed_a) <- runParser pa i
      case runParser pb rem_a of
        Just (rem_b, _) -> return (rem_b, parsed_a)
        Nothing -> return out_a

(+++) :: Parser [a] -> Parser [a] -> Parser [a] -- chains together two string parsers and concatenates their parsed outputs
pa +++ pb = Parser f
  where
    f i = do
      (rem_a, parsed_a) <- runParser pa i
      (rem_b, parsed_b) <- runParser pb rem_a
      Just (rem_b, parsed_a ++ parsed_b)

(*++) :: Parser a -> Parser [a] -> Parser [a] -- chains together a char parser and a string parser and concatenates their parsed outputs
pa *++ pb = Parser f
  where
    f i = do
      (rem_a, parsed_a) <- runParser pa i
      (rem_b, parsed_b) <- runParser pb rem_a
      Just (rem_b, parsed_a : parsed_b)

(++*) :: Parser [a] -> Parser a -> Parser [a] -- chains together a string parser and a char parser and concatenates their parsed outputs
pa ++* pb = Parser f
  where
    f i = do
      (rem_a, parsed_a) <- runParser pa i
      (rem_b, parsed_b) <- runParser pb rem_a
      Just (rem_b, parsed_a ++ [parsed_b])

(?++) :: Parser [a] -> Parser [a] -> Parser [a] -- works like +++ if both parsers succeed, makes the parser on the question mark side optional though
pa ?++ pb = Parser f
  where
    f i = case runParser pa i of
      Just (rem_a, parsed_a) -> do
        (rem_b, parsed_b) <- runParser pb rem_a
        Just (rem_b, parsed_a ++ parsed_b)
      Nothing -> do
        out_b@(rem_b, parsed_b) <- runParser pb i
        Just out_b

(++?) :: Parser [a] -> Parser [a] -> Parser [a]
pa ++? pb = Parser f
  where
    f i = do
      out_a@(rem_a, parsed_a) <- runParser pa i
      case runParser pb rem_a of
        Just (rem_b, parsed_b) -> Just (rem_b, parsed_a ++ parsed_b)
        Nothing -> Just out_a

(?++?) :: Parser [a] -> Parser [a] -> Parser [a] -- both sides are optional - in case both fail an empty list is returned with the input string as remains
pa ?++? pb = Parser f
  where
    f i = case runParser pa i of
      out_a@(Just (rem_a, parsed_a)) -> case runParser pb rem_a of
        Just (rem_b, parsed_b) -> Just(rem_b, parsed_a ++ parsed_b)
        Nothing -> out_a 
      Nothing -> case runParser pb i of
        out_b@(Just (rem_b, parsed_b)) -> out_b
        Nothing -> Just (i, [])

(++?*) :: Parser [a] -> Parser a -> Parser [a] -- like ?++ or ++? but with syntactic sugar for stringify, might reimplement some with (:) later
pa ++?* pb = pa ++? stringify pb

(*++?) :: Parser a -> Parser [a] -> Parser [a]
pa *++? pb = stringify pa ++? pb

(*?++) :: Parser a -> Parser [a] -> Parser [a]
pa *?++ pb = stringify pa ?++ pb

(?++*) :: Parser [a] -> Parser a -> Parser [a]
pa ?++* pb = pa ?++ stringify pb
