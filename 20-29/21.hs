--21.hs Insert an element at a given position

insertAt :: a -> [a] -> Int -> [a]
insertAt elem list idx 
              | idx > length list = list
              | otherwise = take (idx-1) list ++ [elem] ++ drop (idx-1) list