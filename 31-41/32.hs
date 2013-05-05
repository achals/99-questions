-- 32.hs GCD between two positive integers using Euclids Algorithm

myGCD :: Int -> Int -> Int
myGCD num1 num2
      | num1 == num2 = num1
      | num1 > num2 = myGCD (num1-num2) num2
      | otherwise = myGCD num1 (num2-num1)