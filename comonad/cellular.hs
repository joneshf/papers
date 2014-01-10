module Cellular where

import Control.Comonad

data U x = U [x] x [x] deriving (Eq)

instance Show x => Show (U x) where
    show (U a b c) =
        show (take 10 a) ++ " (" ++ show b ++ ") " ++ show (take 10 c)

instance Functor U where

    fmap f (U a b c) = U (fmap f a) (f b) (fmap f c)

instance Comonad U where

    extract (U _ b _) = b

    duplicate u = U (tail $ iterate left u) u (tail $ iterate right u)

left :: U x -> U x
left (U (a:as) b cs) = U (as) a (b:cs)
left u               = u

right :: U x -> U x
right (U as b (c:cs)) = U (b:as) c cs
right u               = u

rule :: U Bool -> Bool
rule (U (a:_) b (c:_)) = (a && not c) || ((not a) && c)

shift :: Int -> U x -> U x
shift i u = (iterate (if i < 0 then left else right) u) !! abs i

toList :: Int -> Int -> U x -> [x]
toList i j = take (j - i) . half . shift i
    where half (U _ b c) = [b] ++ c

bools :: U Bool
bools = U (repeat False) True (repeat False)

showBool :: Bool -> Char
showBool True  = '@'
showBool False = ' '

showBools :: Int -> U Bool -> String
showBools n = map showBool . toList (-n) n

test :: Int -> IO ()
test n = putStr .
         unlines .
         take n .
         map (showBools n) $
         iterate (=>> rule) bools