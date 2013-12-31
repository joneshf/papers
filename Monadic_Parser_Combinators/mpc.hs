{-# LANGUAGE  FlexibleContexts
            , FlexibleInstances
            , FunctionalDependencies
            , NoMonomorphismRestriction
            , UndecidableInstances
            #-}

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

--item :: Parser Char
--item = Parser $ \inp -> case inp of
--    [] -> []
--    x:xs -> [(x, xs)]

-- In case we wanted seq anyway.
seq :: MonadPlus m => m a -> m b -> m (a, b)
p `seq` q = p >>= \x ->
            q >>= \y ->
            return (x, y)

-- Combinator to test a specific character.
sat :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    guard $ p x
    return x

-- Actual parsers

char :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Char -> Parser Char
char c = sat (==c)

digit :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Char
digit = sat (\d -> '0' <= d && d <= '9')

lower :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Char
lower = sat (\c -> 'a' <= c && c <= 'z')

upper :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Char
upper = sat (\c -> 'A' <= c && c <= 'Z')

-- More parsers

letter :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Char
letter = lower `mplus` upper

alphaNum :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Char
alphaNum = letter `mplus` digit

word :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser String
word = nonEmpty `mplus` return ""
    where
        nonEmpty = letter >>= \x  ->
                   word   >>= \xs ->
                   return (x:xs)

-- Match specific strings.
string :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => String -> Parser String
string ""     = return ""
string (c:cs) = do
    char c
    string cs
    return (c:cs)

-- More combinators

-- This could be made specific to Parsers,
-- but it's a fun exercise to generalize it.
many :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => MonadPlus m => m a -> m [a]
many p = do
    x <- p
    xs <- many p
    return (x:xs)
    `mplus` return []

ident :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser String
ident = do
    x <- lower
    xs <- many' alphaNum
    return (x:xs)

-- This seems to just take the init of many, so why not define it this way?
many1 :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => MonadPlus m => m a -> m [a]
many1 p = do
    y <- p
    ys <- many p
    return (y:ys)

int :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Int
int = do
    char '-'
    n <- nat
    return $ -n
    `mplus` nat

sepBy1 :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => MonadPlus m => m a -> m b -> m [a]
p `sepBy1` sep = do
    x <- p
    xs <- many $ do
        sep
        p
    return (x:xs)

bracket :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => MonadPlus m => m a -> m b -> m c -> m b
bracket open p close = do
    open
    x <- p
    close
    return x

ints :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser [Int]
ints = bracket (char '[')
               (int `sepBy1` char ',')
               (char ']')

sepBy :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => MonadPlus m => m a -> m b -> m [a]
p `sepBy` sep = (p `sepBy1` sep) `mplus` return []

-- Simple arithmetic expression parser.

expr :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Int
expr = term `chainl1` addOp

term :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Int
term = factor `chainr1` expOp

expOp :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser (Int -> Int -> Int)
expOp = ops [(char '^', (^))]

factor :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Int
factor = nat `mplus` bracket (char '(') expr (char ')')

addOp :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser (Int -> Int -> Int)
addOp = ops [(char '+', (+)), (char '-', (-))]

-- Construct operators easier.
ops :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => MonadPlus m => [(m a, b)] -> m b
ops xs = foldr1 mplus $ do
    (p, op) <- xs
    return $ do
        p
        return op

-- More efficient?
chainl1 :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => MonadPlus m => m a -> m (a -> a -> a) -> m a
p `chainl1` op = do
    z <- p
    rest z
    where
        rest x = do
            f <- op
            y <- p
            rest (x `f` y)
            `mplus` return x

chainr1 :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => MonadPlus m => m a -> m (a -> a -> a) -> m a
p `chainr1` op = do
    z <- p
    do
        f <- op
        y <- p `chainr1` op
        return $ z `f` y
        `mplus` return z

chainl :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainl p op z = p `chainl1` op `mplus` return z

chainr :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => MonadPlus m => m a -> m (a -> a -> a) -> a -> m a
chainr p op z = p `chainr1` op `mplus` return z

nat :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Int
nat = do
    d <- digit
    return $ ord d - ord '0'
    `chainl1` return op
    where
        m `op` n = 10*m + n

-- Efficiency

eval :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Int
eval = do
    x <- nat
    op <- ops [(char '+', (+)), (char '-', (-))]
    y <- nat
    return $ x `op` y

-- Would be nice to generalize these to work over monadplus's
force :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser a -> Parser a
force p = Parser $ \inp ->
    let
        x = deParse p inp
    in
        head x : tail x

-- Strict version of many specific to parsers.
many' :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser a -> Parser [a]
many' p = force $ do
    x <- p
    xs <- many' p
    return $ x:xs
    +++ return []

first :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser a -> Parser a
first p = Parser $ \inp -> case deParse p inp of
    [] -> []
    x:_ -> [x]

-- Deterministic choice
(+++) :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser a -> Parser a -> Parser a
p +++ q = first $ p `mplus` q

number :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Int
number = nat +++ return 0

color :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser String
color = p1 +++ p2
    where
        p1 = string "yellow"
        p2 = string "orange"

-- Lexing

spaces :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser ()
spaces = do
    many1 $ sat isSpace
    return ()

comment :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser ()
comment = do
    string "--"
    many' $ sat (/= '\n')
    return ()

-- Is this the desired solution?
multiComment :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser ()
multiComment = do
    bracket (string "{-")
            (many' $ sat $ const True)
            (string "-}")
    return ()

--junk :: Parser ()
--junk = do
--    many' $ spaces +++ comment
--    return ()

parse :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser a -> Parser a
parse p = do
    junk
    p

token :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser a -> Parser a
token p = do
    v <- p
    junk
    return v

natural :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Int
natural = token nat

integer :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Int
integer = token int

symbol :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => String -> Parser String
symbol = token . string

identifier :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => [String] -> Parser String
identifier xs = token $ do
    x <- ident
    guard $ x `notElem` xs
    return x

-- Lambda-exp's

data Expr = App Expr Expr           -- application
          | Lam String Expr         -- lambda abstraction
          | Let [(String, Expr)] Expr    -- local definition
          | Var String              -- variable
          | Nat Int
          deriving Show

lexpr :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Expr
lexpr = atom `chainl1` return App

atom :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Expr
atom =  lam
    +++ local
    +++ var
    +++ num
    +++ paren

lam :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Expr
lam = do
    symbol "\\"
    x <- variable
    symbol "->"
    e <- lexpr
    return $ Lam x e

local :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Expr
local = do
    symbol "let"
    ds <- many1Offside defn
    symbol "in"
    body <- lexpr
    return $ Let ds body

defn :: (ReaderMonad Parser Pos, StateMonad Parser Pstring)
     => Parser (String, Expr)
defn = do
    x <- variable
    symbol "="
    e <- lexpr
    return (x, e)

var :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Expr
var = do
    x <- variable
    return $ Var x

num :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Expr
num = do
    d <- nat
    return $ Nat d

paren :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser Expr
paren = bracket (symbol "(") lexpr (symbol ")")

variable :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser String
variable = identifier ["let", "in"]

-- Monads
data Option a = Some a | None

instance Monad Option where

    return = Some

    Some x >>= f = f x
    None   >>= _ = None

instance MonadPlus Option where

    mzero = None

    Some x `mplus` _ = Some x
    None   `mplus` x = x

data List a = Nil | Cons a (List a)

instance Monad List where

    return x = Cons x Nil

    Nil       >>= _ = Nil
    Cons x xs >>= f = f x `mplus` (xs >>= f)

instance MonadPlus List where

    mzero = Nil

    Nil         `mplus` x  = x
    (Cons x xs) `mplus` ys = Cons x (xs `mplus` ys)

newtype State s a = State {getState :: s -> (a, s) }

instance Monad (State s) where

    return x = State $ \s -> (x, s)

    -- This is super confusing:
    -- We apply our first s -> (a, s) to whatever we take in.
    -- This gives us a new (value, state) tuple,
    -- so we apply f to our value, which gives us a new State,
    -- then we update this state with the state from our first application.
    -- Finally, we wrap everything up in a State.
    State st >>= f = State $ \s ->
        let
            (v, s') = st s
            State s'' = f v
        in
            s'' s'

class Monad m => StateMonad m s | m -> s where

    -- This is the only thing that needs to be implemented.
    update :: (s -> s) -> m s

    set :: s -> m s
    set = update . const

    fetch :: m s
    fetch = update id

instance StateMonad (State s) s where

    update f = State $ \s -> (s, f s)

newtype StateM m s a = StateM { getStateM :: s -> m (a, s) }

instance Monad m => Monad (StateM m s) where

    return x = StateM $ \s -> return (x, s)

    -- No worldly clue.
    -- But it should be threading them together.
    StateM stm >>= f = StateM $ stm >=> (\(v, s) -> getStateM (f v) s)

instance MonadPlus m => MonadPlus (StateM m s) where

    mzero = StateM $ const mzero

    StateM stma `mplus` StateM stmb = StateM $ \s -> stma s `mplus` stmb s

instance Monad m => StateMonad (StateM m s) s where

    update f = StateM $ \s -> return (s, f s)

newtype Parser' a = Parser' { getParser' :: StateM [] String a }

-- This is getting unwieldy.
item' :: StateMonad m [b] => m b
item' = do
    x:_ <- update tail
    return x

-- More lexing.

-- Line and column along with the string being parsed.
type Pstring = (Pos, String)

-- Line and column being parsed.
type Pos = (Int, Int)

-- New Parser. Has reader and state monads, and carries position information.
newtype MonParser a =
    MonParser { getMonParser :: ReaderM (StateM [] Pstring) Pos a}

-- Parameterized state reader.
-- Should be a MonadPlus.
newtype ReaderM m s a = ReaderM { getReaderM :: s -> m a }

instance Monad m => Monad (ReaderM m s) where
    return = ReaderM . const . return

    ReaderM srm >>= f = ReaderM $ \s -> do
        v <- srm s
        getReaderM (f v) s

instance MonadPlus m => MonadPlus (ReaderM m s) where

    mzero = ReaderM $ const mzero

    ReaderM srma `mplus` ReaderM srmb = ReaderM $ \s -> srma s `mplus` srmb s

-- Reader monad.
class Monad m => ReaderMonad m s | m -> s where

    env :: m s
    setEnv :: s -> m a -> m a

-- Can read and set the environment.
instance Monad m => ReaderMonad (ReaderM m s) s where

    env = ReaderM $ \s -> return s

    setEnv s (ReaderM srm) = ReaderM $ return $ srm s

-- Can read, set and update the state.
instance StateMonad m a => StateMonad (ReaderM m s) a where

    update = ReaderM . const . update

-- Gotta update item once again.
item :: (MonadPlus m, ReaderMonad m Pos, StateMonad m Pstring) => m Char
item = do
    (pos, x:_) <- update newstate
    defpos <- env
    guard $ onside pos defpos
    return x

tabSize :: Int
tabSize = 4

onside :: Pos -> Pos -> Bool
onside (l, c) (dl, dc) = c > dc || l == dl

newstate :: Pstring -> Pstring
newstate ((l, c), s:ss) = (pos, ss)
    where
        pos = case s of
            '\n' -> (l + 1, 0)
            '\t' -> (l, ((c `div` tabSize) + 1) * tabSize)
            _    -> (l, c + 1)

junk :: (ReaderMonad Parser Pos, StateMonad Parser Pstring) => Parser ()
junk = do
    setEnv (0, -1) (many (spaces +++ comment))
    return ()

many1Offside :: (ReaderMonad Parser Pos, StateMonad Parser Pstring)
             => Parser a -> Parser [a]
many1Offside p = do
    (pos,_) <- fetch
    setEnv pos (many1 (off p))

off :: (ReaderMonad Parser Pos, StateMonad Parser Pstring)
    => Parser a -> Parser a
off p = do
    (_, dc) <- env
    ((l, c), _) <- fetch
    guard $ c == dc
    setEnv (l, dc) p

manyOffside :: (ReaderMonad Parser Pos, StateMonad Parser Pstring)
            => Parser a -> Parser [a]
manyOffside p = many1Offside p +++ return []
