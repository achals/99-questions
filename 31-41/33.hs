-- 33.hs Determine if two number are coprime, i.e. their GCD is 1.

myGCD :: Int -> Int -> Int
myGCD num1 num2
      | num1 == num2 = num1
      | num1 > num2 = myGCD (num1-num2) num2
      | otherwise = myGCD num1 (num2-num1)

coprime :: Int -> Int -> Bool
coprime num1 num2 = myGCD num1 num2 == 1