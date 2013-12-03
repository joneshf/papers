module Deter where

import Prelude hiding (seq)

-- A parser takes a String and returns a list of `(a, String)` 2-tuples.
-- If the list is empty, the parser failed.
-- Should this be implemented as some other ADT?
newtype Parser a = Parser (String -> [(a, String)])

-- Need a way to deconstruct parsers.
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Monad Parser where
    return v = Parser $ \inp -> [(v, inp)]
    p >>= f  = Parser $ \inp ->
        concat [parse (f v) inp' | (v, inp') <- parse p inp]

-- Primitive parsers.
-- These are defined this way to follow closer the type signature.

zero :: Parser a
zero = Parser $ const []

item :: Parser Char
item = Parser $ \inp -> case inp of
    [] -> []
    x:xs -> [(x, xs)]

-- In case we wanted seq anyway.
seq :: Parser a -> Parser b -> Parser (a, b)
p `seq` q = p >>= \x ->
            q >>= \y ->
            return (x, y)

-- Combinator to test a specific character.
sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
    if p x then return x else zero

-- Actual parsers

char :: Char -> Parser Char
char c = sat (==c)

digit :: Parser Char
digit = sat (\d -> '0' <= d && d <= '9')

lower :: Parser Char
lower = sat (\c -> 'a' <= c && c <= 'z')

upper :: Parser Char
upper = sat (\c -> 'A' <= c && c <= 'Z')

-- Choice operator
plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ \inp -> parse p inp ++ parse q inp

-- More parsers

letter :: Parser Char
letter = lower `plus` upper

alphaNum :: Parser Char
alphaNum = letter `plus` digit

word :: Parser String
word = nonEmpty `plus` return ""
    where
        nonEmpty = letter >>= \x  ->
                   word   >>= \xs ->
                   return (x:xs)
