-- K'th element in a list
elemAt :: [a] -> Int -> a
elemAt (x:_) 1 = x
elemAt [] _ = error "Empty list reached."
elemAt (_:xs) y 
	| y > 1 = elemAt xs (y-1)
	| otherwise = error "Index out of bounds"
