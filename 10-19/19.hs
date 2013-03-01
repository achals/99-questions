-- 19.hs Rotate a list by N places.

rotate :: [a] -> Int -> [a]
rotate list num = helper list offset
       where 
       offset = mod num (length list)
       helper :: [a] -> Int -> [a]
       helper list offset
              | offset == 0 = list
              | offset > 0 = (drop offset list) ++ (take offset list)
              | offset < 0 = helper list (offset + (length list))