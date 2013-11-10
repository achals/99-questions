-- 55.hs
-- Construct complete balanced binary tree.

data Tree a = Leaf a | Branch (Tree a) (Tree a)
     deriving Show

cbalTree :: Int -> Tree Char
cbalTree = undefined