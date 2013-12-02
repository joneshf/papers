module Deter where

import Prelude hiding (seq)

-- A parser takes a String and returns a list of `(a, String)` 2-tuples.
-- If the list is empty, the parser failed.
-- Should this be implemented as some other ADT?
type Parser a = String -> [(a, String)]


-- Cant actually do this yet.
--instance Monad Parser where
--    return = result
--    (>>=) = bind

-- Primitive parsers.
-- These are defined this way to follow closer the type signature.
result :: a -> Parser a
result v = \inp -> [(v, inp)]

zero :: Parser a
zero = \inp -> []

item :: Parser Char
item = \inp -> case inp of
    [] -> []
    x:xs -> [(x, xs)]

-- Our sequencing operator.
-- Easier to work with than seq.
bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp' | (v, inp') <- p inp]

-- In case we wanted seq anyway.
seq :: Parser a -> Parser b -> Parser (a, b)
p `seq` q = p `bind` \x ->
            q `bind` \y ->
            result (x, y)

-- Combinator to test a specific character.
sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` \x ->
    if p x then result x else zero

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
p `plus` q = \inp -> p inp ++ q inp

-- More parsers

letter :: Parser Char
letter = lower `plus` upper

alphaNum :: Parser Char
alphaNum = letter `plus` digit

word :: Parser String
word = nonEmpty `plus` result ""
    where
        nonEmpty = letter `bind` \x  ->
                   word   `bind` \xs ->
                   result (x:xs)
