-- 39.hs A list of prime numbers in a specified range.


isPrime :: Int -> Bool
isPrime x = helper 2 x
        where 
        helper :: Int -> Int -> Bool
        helper n x
               | n > (x `div` 2) = True
               | (x `mod` n) == 0  = False
               | otherwise  = helper (n+1) x


primeR :: Int -> Int -> [Int]
primeR start end = [x | x <- [start..end], isPrime x]

