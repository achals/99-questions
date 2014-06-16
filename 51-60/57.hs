-- 57.hs
-- Binary search trees.

data Tree a = Branch a (Tree a) (Tree a) | Empty
     deriving (Show)

add :: Ord a => a -> Tree a -> Tree a
add num Empty = Branch num Empty Empty
add num tree@(Branch orig left right)
        | num == orig = tree
        | num < orig = Branch orig ( add num left ) right
        | num > orig = Branch orig left (add num right)

construct :: Ord a => [a] -> Tree a
construct = foldr add Empty