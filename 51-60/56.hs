-- 56.hs
-- Symmetric Binary Trees

data Tree a = Branch a (Tree a) (Tree a) | Empty
     deriving (Show)

symmetricTrees :: Tree a -> Tree a -> Bool
symmetricTrees Empty Empty = True
symmetricTrees (Branch _ left1 right1) (Branch _ left2 right2) = (symmetricTrees left1 right2) && (symmetricTrees left2 right1)
symmetricTrees _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = symmetricTrees left right