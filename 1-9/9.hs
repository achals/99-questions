--Seperate duplicate elements into seperate sublists.
pack:: Eq a=> [a]->[[a]]
pack []=[]
pack (x:xs) = (x:first) : pack rest
	where
		getReps []=([], [])
		getReps (y:ys)
			| y==x = let (f, r) = getReps ys in ((y:f), r)
			| otherwise =([], (y:ys))
		(first, rest) = getReps xs

