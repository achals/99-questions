-- 40.hs Goldbach's conjecture
-- Find two prime numbers that sum up to a given number 


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