module Deter where

import Prelude hiding (seq)
import Control.Monad

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

instance MonadPlus Parser where
    mzero = Parser $ const []
    p `mplus` q = Parser $ \inp -> parse p inp ++ parse q inp

-- Primitive parsers.
-- These are defined this way to follow closer the type signature.

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
sat p = do
    x <- item
    if p x then return x else mzero

-- Actual parsers

char :: Char -> Parser Char
char c = sat (==c)

digit :: Parser Char
digit = sat (\d -> '0' <= d && d <= '9')

lower :: Parser Char
lower = sat (\c -> 'a' <= c && c <= 'z')

upper :: Parser Char
upper = sat (\c -> 'A' <= c && c <= 'Z')

-- More parsers

letter :: Parser Char
letter = lower `mplus` upper

alphaNum :: Parser Char
alphaNum = letter `mplus` digit

word :: Parser String
word = nonEmpty `mplus` return ""
    where
        nonEmpty = letter >>= \x  ->
                   word   >>= \xs ->
                   return (x:xs)

-- Match specific strings.
string :: String -> Parser String
string ""     = return ""
string (c:cs) = do
    char c
    string cs
    return (c:cs)

-- More combinators

-- This could be made specific to Parsers,
-- but it's a fun exercise to generalize it.
many :: MonadPlus m => m a -> m [a]
many p = do
    x <- p
    xs <- many p
    return (x:xs)
    `mplus` return []

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphaNum
    return (x:xs)

-- This seems to just take the init of many, so why not define it this way?
many1 :: MonadPlus m => m a -> m [a]
many1 p = do
    y <- p
    ys <- many p
    return (y:ys)

nat :: Parser Int
nat = do
    ns <- many1 digit
    return $ read ns

int :: Parser Int
int = do
    char '-'
    n <- nat
    return $ -n
    `mplus` nat
-- More "sophisticated".
--int = do
--    f <- op
--    n <- nat
--    return $ f n
--        where
--            op = do
--                char '-'
--                return negate
--                `mplus` return id
