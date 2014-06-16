-- 58.hs
-- Generate and test paradigm.

data Tree a = Branch a (Tree a) (Tree a) | Empty
     deriving (Show)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n-1) `quotRem` 2
             in [Branch 'x' left right | i <- [q .. q+r],
                                         left <- cbalTree i,
                                         right <- cbalTree ( n - 1 - i ) ]
symmetricTrees :: Tree a -> Tree a -> Bool
symmetricTrees Empty Empty = True
symmetricTrees (Branch _ left1 right1) (Branch _ left2 right2) = (symmetricTrees left1 right2) && (symmetricTrees left2 right1)
symmetricTrees _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = symmetricTrees left right

symCbalTree :: Int -> [Tree Char]
symCbalTree = filter symmetric . cbalTree