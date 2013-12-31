module Expressions where

data Expr = Val Int
          | Add Expr Expr
    deriving Show

type Stack = [Either Expr Int]

eval :: Expr -> Int
eval e = load e []

load :: Expr -> Stack -> Int
load (Val i)     stack = unload i stack
load (Add e1 e2) stack = load e1 (Left e2 : stack)

unload :: Int -> Stack -> Int
unload v  []                 = v
unload v  (Left  e  : stack) = load e (Right v : stack)
unload v2 (Right v1 : stack) = unload (v1 + v2) stack
