-- 41.hs list of goldbach compositions for even numbers in a given range.


isPrime :: Int -> Bool
isPrime x = helper 2 x
        where 
        helper :: Int -> Int -> Bool
        helper n x
               | n > (x `div` 2) = True
               | (x `mod` n) == 0  = False
               | otherwise  = helper (n+1) x

goldbach :: Int -> (Int, Int)
goldbach num = pair 
         where 
         tempList =  [(x, num-x) | x <- [2..num-1], isPrime x, isPrime (num-x)]
         pair = if (length tempList == 0) 
                then (1, num-1)
                else (head tempList)

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList low high = map goldbach $ filter even [low..high]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' low high limit  =  filter (\x -> fst x > limit) $ goldbachList low high