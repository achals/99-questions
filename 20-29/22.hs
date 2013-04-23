-- 22.hs Create a list of integers in a given range

range :: Int -> Int -> [Int]
range start end
      | start > end = []
      |otherwise = start : range (start + 1) end