module Deter where

import Prelude hiding (seq)
import Control.Monad
import Data.Char

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
seq :: MonadPlus m => m a -> m b -> m (a, b)
p `seq` q = p >>= \x ->
            q >>= \y ->
            return (x, y)

-- Combinator to test a specific character.
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    guard $ p x
    return x

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

--nat :: Parser Int
--nat = do
--    ns <- many1 digit
--    return $ read ns

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

sepBy1 :: MonadPlus m => m a -> m b -> m [a]
p `sepBy1` sep = do
    x <- p
    xs <- many $ do
        sep
        p
    return (x:xs)

bracket :: MonadPlus m => m a -> m b -> m c -> m b
bracket open p close = do
    open
    x <- p
    close
    return x

ints :: Parser [Int]
ints = bracket (char '[')
               (int `sepBy1` char ',')
               (char ']')

sepBy :: MonadPlus m => m a -> m b -> m [a]
p `sepBy` sep = (p `sepBy1` sep) `mplus` return []

-- Simple arithmetic expression parser.

expr :: Parser Int
expr = term `chainl1` addOp

term :: Parser Int
term = factor `chainr1` expOp

expOp :: Parser (Int -> Int -> Int)
expOp = ops [(char '^', (^))]

factor :: Parser Int
factor = nat `mplus` bracket (char '(') expr (char ')')

addOp :: Parser (Int -> Int -> Int)
addOp = ops [(char '+', (+)), (char '-', (-))]

-- Construct operators easier.
ops :: MonadPlus m => [(m a, b)] -> m b
ops xs = foldr1 mplus $ do
    (p, op) <- xs
    return $ do
        p
        return op

-- More efficient?
chainl1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
p `chainl1` op = do
    z <- p
    rest z
    where
        rest x = do
            f <- op
            y <- p
            rest (x `f` y)
            `mplus` return x

chainr1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
p `chainr1` op = do
    z <- p
    do
        f <- op
        y <- p `chainr1` op
        return $ z `f` y
        `mplus` return z

chainl :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainl p op z = p `chainl1` op `mplus` return z

chainr :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainr p op z = p `chainr1` op `mplus` return z

nat :: Parser Int
nat = do
    d <- digit
    return $ ord d - ord '0'
    `chainl1` return op
    where
        m `op` n = 10*m + n

-- Efficiency

eval :: Parser Int
-- Backtracking
--eval = add `mplus` sub
--    where
--        add = do
--            x <- nat
--            char '+'
--            y <- nat
--            return $ x + y
--        sub = do
--            x <- nat
--            char '-'
--            y <- nat
--            return $ x - y

-- Linear time
--eval = do
--    x <- nat
--    add x `mplus` sub x
--    where
--        add x = do
--            char '+'
--            y <- nat
--            return $ x + y
--        sub x = do
--            char '-'
--            y <- nat
--            return $ x - y

-- Easy on the eyes.
eval = do
    x <- nat
    op <- ops [(char '+', (+)), (char '-', (-))]
    y <- nat
    return $ x `op` y

-- Would be nice to generalize these to work over monadplus's
force :: Parser a -> Parser a
force p = Parser $ \inp ->
    let
        x = parse p inp
    in
        head x : tail x

-- Strict version of many specific to parsers.
many' :: Parser a -> Parser [a]
many' p = force $ do
    x <- p
    xs <- many' p
    return $ x:xs
    `mplus` return []

first :: Parser a -> Parser a
first p = Parser $ \inp -> case parse p inp of
    [] -> []
    x:_ -> [x]

-- Deterministic choice
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first $ p `mplus` q

number :: Parser Int
number = nat +++ return 0

color :: Parser String
color = p1 +++ p2
    where
        p1 = string "yellow"
        p2 = string "orange"
