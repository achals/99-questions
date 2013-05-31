-- 36.hs Determine prime factors of a number, listing prime factors and their multiplicity

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


prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = prime_factors_mult' primes
                   where
                   primes = primeFactors n
                   prime_factors_mult' :: [Int] -> [(Int, Int)]
                   prime_factors_mult' [] = []
                   prime_factors_mult' list = (head list, length $ takeWhile (== head list) list) : prime_factors_mult'  (dropWhile (== head list) list) 


--prime_factors_mult'' n = map swap $ encode $ primeFactors n
--  where swap (x,y) = (y,x)
--This was in the solutions - elegant