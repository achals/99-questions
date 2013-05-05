-- 34.hs Calculate Eulers totient function

myGCD :: Int -> Int -> Int
myGCD num1 num2
      | num1 == num2 = num1
      | num1 > num2 = myGCD (num1-num2) num2
      | otherwise = myGCD num1 (num2-num1)

coprime :: Int -> Int -> Bool
coprime num1 num2 = myGCD num1 num2 == 1

totient :: Int -> Int
totient x = length (filter (coprime x) [1..x-1])