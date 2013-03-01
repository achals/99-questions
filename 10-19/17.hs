--17.hs Split a list into two parts without using any predefined predicates.

split :: [a] -> Int -> ([a], [a])
split list num = helper [] list num
      where
      helper :: [a] -> [a] -> Int -> ([a], [a])
      helper first second 0 = (reverse first, second)
      helper first [] _  = (first, [])
      helper first (x:xs) n = helper (x:first) xs (n-1)

split' :: [a] -> Int -> ([a], [a])
split' list num = 