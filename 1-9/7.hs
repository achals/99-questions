-- Flatteing a tree structure stored as a list of lists.
-- Initiall thought of this: myFlatten :: [[a]] -> [a]
-- But wrong, because this just makes it a list of lists, very narrow as compared to a tree.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten (x) ++ flatten (List xs)
flatten (List []) = []
