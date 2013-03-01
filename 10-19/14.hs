-- 14.hs duplicate elements in a list
-- Probably a bunch of ways to do this. Resultant list should necessarily have an even length.

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x: (dupli xs)

dupli' :: [a] -> [a]
dupli' list = concat (map (replicate 2) list)

dupli'' :: [a] -> [a]
dupli'' list = foldr (\x y -> x:x:y) [] list