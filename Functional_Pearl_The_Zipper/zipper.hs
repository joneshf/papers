module Zipper where

data Zipper a = Zipper ([a], a, [a])
    deriving (Eq, Show)

left :: Zipper a -> Zipper a
left (Zipper (as, b, c:cs)) = Zipper (b:as, c, cs)
left z             = z

right :: Zipper a -> Zipper a
right (Zipper (a:as, b, cs)) = Zipper (as, a, b:cs)
right z                      = z

toList :: Zipper a -> [a]
toList (Zipper ([], b, cs)) = b:cs
toList z                    = toList . right $ z

component :: Eq a => a -> Zipper a -> Bool
component a (Zipper (_, b, [])) | a /= b = False
component a (Zipper (as, b, c:cs))
    | b == a    = True
    | otherwise = component a (Zipper (b:as, c, cs))

modify :: Eq a => (a -> a) -> a -> Zipper a -> Zipper a
modify _ a z@(Zipper (_, b, []))
    | a /= b    = z
modify f a (Zipper (as, b, c:cs))
    | a == b    = modify f a (Zipper (f b : as, c, cs))
    | otherwise = modify f a (Zipper (b : as, c, cs))
