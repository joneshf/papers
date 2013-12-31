module BinaryZipper where

data BinaryTree = Nil
                | Cons (BinaryTree, BinaryTree)
    deriving Show

data BinaryPath = Top
                | LeftPath (BinaryPath, BinaryTree)
                | RightPath (BinaryTree, BinaryPath)
    deriving Show

data BinaryLocation = Loc (BinaryTree, BinaryPath)

change :: BinaryTree -> BinaryLocation -> BinaryLocation
change t (Loc (_, p)) = Loc (t, p)

goLeft :: BinaryLocation -> BinaryLocation
goLeft (Loc (_, Top))              = error "Left of Top"
goLeft (Loc (_, LeftPath _))       = error "Left of Left"
goLeft (Loc (t, RightPath (l, u))) = Loc (l, LeftPath (u, t))

goRight :: BinaryLocation -> BinaryLocation
goRight (Loc (_, Top))             = error "Right of Top"
goRight (Loc (_, RightPath _))     = error "Right of Right"
goRight (Loc (t, LeftPath (u, r))) = Loc (r, RightPath (t, u))

goUp :: BinaryLocation -> BinaryLocation
goUp (Loc (_, Top))              = error "Up of Top"
goUp (Loc (t, LeftPath (u, r)))  = Loc (Cons (t, r), u)
goUp (Loc (t, RightPath (l, u))) = Loc (Cons (l, t), u)

goFirst :: BinaryLocation -> BinaryLocation
goFirst (Loc (Nil, _))         = error "First of Nil"
goFirst (Loc (Cons (l, r), u)) = Loc (l, LeftPath (u, r))

goSecond :: BinaryLocation -> BinaryLocation
goSecond (Loc (Nil, _))         = error "Second of Nil"
goSecond (Loc (Cons (l, r), u)) = Loc (r, RightPath (l, u))
