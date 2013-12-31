{-# LANGUAGE KindSignatures, RankNTypes #-}
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

-- Polynomial type constructors
data K1       a   x = K1 a                  deriving Show
data Id           x = Id x                  deriving Show
data Sum1     p q x = L1 (p x) | R1 (q x)   deriving Show
data Product1 p q x = Pair1 (p x) (q x)     deriving Show

type One1 = K1 ()

type Option = Sum1 One1 Id

none :: Option a
none = L1 (K1 ())

some :: a -> Option a
some x = R1 (Id x)

instance Functor (K1 a) where
    fmap _ (K1 a) = K1 a
instance Functor Id where
    fmap f (Id s) = Id (f s)
instance (Functor p, Functor q) => Functor (Sum1 p q) where
    fmap f (L1 p) = L1 (fmap f p)
    fmap f (R1 q) = R1 (fmap f q)
instance (Functor p, Functor q) => Functor (Product1 p q) where
    fmap f (Pair1 p q) = Pair1 (fmap f p) (fmap f q)

type ExprP = Sum1 (K1 Int) (Product1 Id Id)

-- The types here are too much for me.
--valP :: Int -> ExprP Int
valP :: forall (q :: * -> *) a x. a -> Sum1 (K1 a) q x
valP i = L1 (K1 i)

--addP :: a -> a -> ExprP a
addP :: forall (p :: * -> *) a. a -> a -> Sum1 p (Product1 Id Id) a
addP e1 e2 = R1 (Pair1 (Id e1) (Id e2))

data Mu p = In (p (Mu p))

type Expr1 = Mu ExprP
type AddExpr1 = forall (p :: * -> *). Mu (Sum1 p (Product1 Id Id))
type ValExpr1 = forall (q :: * -> *) a. a -> Mu (Sum1 (K1 a) q)

val :: ValExpr1
val i = In (valP i)

add :: AddExpr1 -> AddExpr1 -> AddExpr1
add e1 e2 = In (addP e1 e2)