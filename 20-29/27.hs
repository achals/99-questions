-- 27.hs Group elements into disjoint subsets

import Control.Applicative

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ combinations n xs

group :: [Int] -> [a] -> [[[a]]]
group sizes items =  ((map combinations) sizes) <*> [items]