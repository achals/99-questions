--35.hs list of prime factors of a positive integer
-- Wait, isn't this NP-Complete?

primeFactors::Int-> [Int]
primeFactors num
             | num==1 = []
             | otherwise = (smallestPrime num) : primeFactors (num `div` (smallestPrime num))

smallestPrime :: Int -> Int
smallestPrime num = head $ filter (\x -> num `mod` x == 0) $ filter isPrime [2..num]

isPrime :: Int -> Bool
isPrime x = helper 2 x
        where 
        helper :: Int -> Int -> Bool
        helper n x
               | n > (x `div` 2) = True
               | (x `mod` n) == 0  = False
               | otherwise  = helper (n+1) x