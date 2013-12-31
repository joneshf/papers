module Zipper where

data Tree i = Item i
            | Section [Tree i]
            deriving Show

data Path a = Top
            | Node ([Tree a], Path a, [Tree a])
            deriving Show

data Location a = Loc (Tree a, Path a)
    deriving Show

-- Parse tree for arithmetic.
--Section [Section [Item "a", Item "*", Item "b"],
--         Item "+",
--         Section [Item "c", Item "*", Item "d"]]

-- I don't much care for these partial functions.
goLeft :: Location a -> Location a
goLeft (Loc (_, Top))               = error "Left of Top"
goLeft (Loc (_, Node ([], _, _)))   = error "Left of First"
goLeft (Loc (t, Node (l:ls, u, r))) = Loc (l, Node (ls, u, t:r))

goRight :: Location a -> Location a
goRight (Loc (_, Top))               = error "Right of Top"
goRight (Loc (_, Node ([], _, _)))   = error "Right of First"
goRight (Loc (t, Node (l, u, r:rs))) = Loc (r, Node (t:l, u, rs))

goUp :: Location a -> Location a
goUp (Loc (_, Top))            = error "Up of Top"
goUp (Loc (t, Node (l, u, r))) = Loc (Section (reverse l ++ t:r), u)

goDown :: Location a -> Location a
goDown (Loc (Item _, _))         = error "Down of Item"
goDown (Loc (Section [], _))     = error "Down of Empty"
goDown (Loc (Section (t:ts), p)) = Loc (t, Node ([], p, ts))
