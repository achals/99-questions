-- Eliminate consecutive duplicates of List elements
-- First thought: define a function to do this for two consecutive List elements, and then use foldl
comp :: Eq a => [a] -> a -> [a]
comp [] x = [x]
comp (x:xs) y
	| (length xs)==0 = [x, y] 
	| (last xs) == y = x:xs
	| otherwise = x:xs ++ [y] 
compress :: (Eq a) => [a] -> [a]
compress = foldl comp [] 

-- Almost there: only had syntax error. Must see what the \a is all about.
--compress :: (Eq a) => [a] -> [a]
--compress x = foldl (\a b -> if (last a) == b then a else a ++ [b]) [head x] x
