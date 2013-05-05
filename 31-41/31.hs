--31.hs Determine whether an integer is prime or not

-- This one has a number of interesting correct solutions, as seen on the solutions
-- page. Very good food for thought

isPrime :: Int -> Bool
isPrime x = helper 2 x
        where 
        helper :: Int -> Int -> Bool
        helper n x
               | n > (x `div` 2) = True
               | (x `mod` n) == 0  = False
               | otherwise  = helper (n+1) x