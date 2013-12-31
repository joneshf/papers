module Lambda where

data Lambda = Var String        -- x
            | Abs String Lambda -- \x.x
            | App Lambda Lambda -- x x
    deriving Show

-- (\x. x) y
-- App (Abs "x" (Var "x")) (Var "y")
-- Var "y"

-- (\x. x (\x.x)) (u r)
-- App (Abs "x" (App (Var "x") (Abs "x" (Var "x")))) (App (Var "u") (Var "r"))
-- reduce (App (Abs "x" (App (Var "x") (Abs "x" (Var "x")))) (App (Var "u") (Var "r")))
-- replace "x" (reduce (App (Var "u") (Var "r"))) (reduce (App (Var "x") (Abs "x" (Var "x"))))
-- replace "x" (App (reduce (Var "u")) (reduce (Var "r"))) (reduce (App (Var "x") (Abs "x" (Var "x"))))
-- replace "x" (App (Var "u") (reduce (Var "r"))) (reduce (App (Var "x") (Abs "x" (Var "x"))))
-- replace "x" (App (Var "u") (Var "r")) (reduce (App (Var "x") (Abs "x" (Var "x"))))
-- replace "x" (App (Var "u") (Var "r")) (App (reduce (Var "x")) (reduce (Abs "x" (Var "x"))))
-- replace "x" (App (Var "u") (Var "r")) (App (Var "x") (reduce (Abs "x" (Var "x"))))
-- replace "x" (App (Var "u") (Var "r")) (App (Var "x") (Abs "x" (reduce (Var "x"))))
-- replace "x" (App (Var "u") (Var "r")) (App (Var "x") (Abs "x" (Var "x")))
-- App (App (Var "u") (Var "r")) (Abs "x" (Var "x"))

reduce :: Lambda -> Lambda
reduce v@Var {}            = v
reduce (Abs s l)           = Abs s (reduce l)
reduce (App (Abs v l1) l2) = replace v (reduce l2) (reduce l1)
reduce (App l1 l2)         = App (reduce l1) (reduce l2)

replace :: String -> Lambda -> Lambda -> Lambda
replace s1 l v@(Var s2)
    | s1 == s2  = l
    | otherwise = v
replace s1 l1 (App (Var s2) l2)
    | s1 == s2  = App l1 l2
    | otherwise = l2
