-- 27.hs Group elements into disjoint subsets

-- I initially got onto the wrong path. Combinations was wrong, which led to group 
-- being wrong. Had to look at the solutions :(


combinations :: Int -> [a] -> [([a], [a])]
combinations 0 xs = [([], xs)]
combinations _ [] = []
combinations n (x:xs) = ts ++ ds
             where 
             ts = [(x:ys, zs) | (ys, zs) <- combinations (n-1) xs]
             ds = [(ys, x:zs) | (ys, zs) <- combinations n xs]

group :: [Int] -> [a] -> [[[a]]]
group _ [] = [[]]
group (n:ns) xs = [front:rest | (front, remaining) <- combinations n xs
                               , rest <- group ns remaining]