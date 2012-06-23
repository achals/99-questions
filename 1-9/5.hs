-- Reversing a list
rev :: [a] -> [a]
rev [x] = [x]
rev (x:xs) = reverse (xs) ++ [x]

-- Really interesting alternative implementation, uses foldl and flip.
-- Flip reverses the order in which the parameters are evaluated
-- foldl applies the specified function to all the elements in a list, element by element, starting with the leftmost. and a specified starting value.

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
