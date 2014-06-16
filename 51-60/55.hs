-- 55.hs
-- Construct complete balanced binary tree.

data Tree a = Branch a (Tree a) (Tree a) | Empty
     deriving Show

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n-1) `quotRem` 2
             in [Branch 'x' left right | i <- [q .. q+r],
                                         left <- cbalTree i,
                                         right <- cbalTree ( n - 1 - i ) ]

cbalTree' :: Int -> [Tree Char]
cbalTree' 0 = [Empty]
cbalTree' n
          | odd n = [ Branch 'x' left right | left <- cbalTree ( ( n -1 ) `div` 2 ),
                                              right <- cbalTree ( ( n -1 ) `div` 2 ) ]
          | otherwise =  concat [ [ Branch 'x' left right, Branch 'x' right left ] | left <- cbalTree ( (n-1) `div` 2 ),
                                                                              right <- cbalTree ( n `div` 1 ) ]