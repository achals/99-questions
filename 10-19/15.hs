-- 15.hs Replicate the elements in a list specified number of times.
-- Can use one of the methods from 14.hs



repli :: [a] -> Int -> [a]
repli list num = concatMap (replicate num) list

repli' :: [a] -> Int -> [a]
repli' list num = foldr (\x y -> replicate num x ++ y) [] list