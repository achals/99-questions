-- 18.hs Extracting a slice from a list

slice :: [a] -> Int -> Int -> [a]
slice list start end
           | start <= end = take (end-start+1) (drop (start-1) list)
           | otherwise = []

-- Great solution found on haskellwiki 

slice' :: [a] -> Int -> Int -> [a]
slice' xs a b = fst $ unzip $ filter ((>=a) . snd) $ zip xs [1..b]
