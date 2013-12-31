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
goRight (Loc (_, Node (_, _, [])))   = error "Right of First"
goRight (Loc (t, Node (l, u, r:rs))) = Loc (r, Node (t:l, u, rs))

goUp :: Location a -> Location a
goUp (Loc (_, Top))            = error "Up of Top"
goUp (Loc (t, Node (l, u, r))) = Loc (Section (reverse l ++ (t:r)), u)

goDown :: Location a -> Location a
goDown (Loc (Item _, _))         = error "Down of Item"
goDown (Loc (Section [], _))     = error "Down of Empty"
goDown (Loc (Section (t:ts), p)) = Loc (t, Node ([], p, ts))

nthChild :: Int -> Location a -> Location a
nthChild 1 location = goDown location
nthChild n location
    | n > 0         = goRight $ nthChild (n-1) location
    | otherwise     = error "nthChild expects positive integer"

change :: Tree a -> Location a -> Location a
change t (Loc (_, p)) = Loc (t, p)

insertRight :: Tree a -> Location a -> Location a
insertRight _ (Loc (_, Top))              = error "Insert of Top"
insertRight t2 (Loc (t, Node (l, u, r)))  = Loc (t, Node (l, u, t2:r))

insertLeft :: Tree a -> Location a -> Location a
insertLeft _ (Loc (_, Top))              = error "Insert of Top"
insertLeft t2 (Loc (t, Node (l, u, r)))  = Loc (t, Node (t2:l, u, r))

insertDown :: Tree a -> Location a -> Location a
insertDown _ (Loc (Item _, _))      = error "Insert of Item"
insertDown t (Loc (Section ts, p))  = Loc (t, Node ([], p, ts))

delete :: Location a -> Location a
delete (Loc (_, Top))                = error "Delete of Top"
delete (Loc (_, Node (l, u, r:rs)))  = Loc (r, Node (l, u, rs))
delete (Loc (_, Node (l:ls, u, []))) = Loc (l, Node (ls, u, []))
delete (Loc (_, Node ([], u, [])))   = Loc (Section [], u)
