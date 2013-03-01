-- 16.hs Drop the nth element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery list n = take (n-1) list ++ dropEvery (drop n list) n