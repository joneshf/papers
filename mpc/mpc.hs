module Deter where

import Prelude hiding (seq)
import Control.Monad
import Data.Char

-- A parser takes a String and returns a list of `(a, String)` 2-tuples.
-- If the list is empty, the parser failed.
-- Should this be implemented as some other ADT?
newtype Parser a = Parser (String -> [(a, String)])

-- Need a way to deconstruct parsers.
deParse :: Parser a -> String -> [(a, String)]
deParse (Parser p) = p

instance Monad Parser where
    return v = Parser $ \inp -> [(v, inp)]
    p >>= f  = Parser $ \inp ->
        concat [deParse (f v) inp' | (v, inp') <- deParse p inp]

instance MonadPlus Parser where
    mzero = Parser $ const []
    p `mplus` q = Parser $ \inp -> deParse p inp ++ deParse q inp

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
letter = lower +++ upper

alphaNum :: Parser Char
alphaNum = letter +++ digit

word :: Parser String
word = nonEmpty +++ return ""
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
--many :: MonadPlus m => m a -> m [a]
--many p = do
--    x <- p
--    xs <- many p
--    return (x:xs)
--    `mplus` return []

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphaNum
    return (x:xs)

-- This seems to just take the init of many, so why not define it this way?
many1 :: Parser a -> Parser [a]
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
    +++ nat
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

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do
    x <- p
    xs <- many $ do
        sep
        p
    return (x:xs)

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
    open
    x <- p
    close
    return x

ints :: Parser [Int]
ints = bracket (char '[')
               (int `sepBy1` char ',')
               (char ']')

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepBy1` sep) +++ return []

-- Simple arithmetic expression parser.

expr :: Parser Int
expr = term `chainl1` addOp

term :: Parser Int
term = factor `chainr1` expOp

expOp :: Parser (Int -> Int -> Int)
expOp = ops [(char '^', (^))]

factor :: Parser Int
factor = nat +++ bracket (char '(') expr (char ')')

addOp :: Parser (Int -> Int -> Int)
addOp = ops [(char '+', (+)), (char '-', (-))]

-- Construct operators easier.
ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 (+++) $ do
    (p, op) <- xs
    return $ do
        p
        return op

-- More efficient?
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
    z <- p
    rest z
    where
        rest x = do
            f <- op
            y <- p
            rest (x `f` y)
            +++ return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = do
    z <- p
    do
        f <- op
        y <- p `chainr1` op
        return $ z `f` y
        +++ return z

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op z = p `chainl1` op +++ return z

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op z = p `chainr1` op +++ return z

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
        x = deParse p inp
    in
        head x : tail x

-- Strict version of many specific to parsers.
many :: Parser a -> Parser [a]
many p = force $ do
    x <- p
    xs <- many p
    return $ x:xs
    +++ return []

first :: Parser a -> Parser a
first p = Parser $ \inp -> case deParse p inp of
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

-- Lexing

spaces :: Parser ()
spaces = do
    many1 $ sat isSpace
    return ()

comment :: Parser ()
comment = do
    string "--"
    many $ sat (/= '\n')
    return ()

-- Is this the desired solution?
multiComment :: Parser ()
multiComment = do
    bracket (string "{-")
            (many $ sat $ const True)
            (string "-}")
    return ()

junk :: Parser ()
junk = do
    many $ spaces +++ comment
    return ()

parse :: Parser a -> Parser a
parse p = do
    junk
    p

token :: Parser a -> Parser a
token p = do
    v <- p
    junk
    return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token . string

identifier :: [String] -> Parser String
identifier xs = token $ do
    x <- ident
    guard $ x `notElem` xs
    return x

-- Lambda-exp's

data Expr = App Expr Expr           -- application
          | Lam String Expr         -- lambda abstraction
          | Let String Expr Expr    -- local definition
          | Var String              -- variable
          deriving Show

lexpr :: Parser Expr
lexpr = atom `chainl1` return App

atom :: Parser Expr
atom =  lam
    +++ local
    +++ var
    +++ paren

lam :: Parser Expr
lam = do
    symbol "\\"
    x <- variable
    symbol "->"
    e <- lexpr
    return $ Lam x e

local :: Parser Expr
local = do
    symbol "let"
    x <- variable
    symbol "="
    e <- lexpr
    symbol "in"
    body <- lexpr
    return $ Let x e body

var :: Parser Expr
var = do
    x <- variable
    return $ Var x

paren :: Parser Expr
paren = bracket (symbol "(") lexpr (symbol ")")

variable :: Parser String
variable = identifier ["let", "in"]
