-- 26.hs Generate combinations of k distinct objects from N objects

-- This one was simpler, but maybe smarter ways of doing it?

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ combinations n xs

combinations' :: Int -> [a] -> [[a]]
combinations' = undefined -- for now :)