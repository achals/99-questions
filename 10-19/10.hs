-- 10.hs Run length encoding
-- Uses the functions from 9.

pack:: Eq a=> [a]->[[a]]
pack []=[]
pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest

encode :: String -> [(Char, Int)]
encode str = map (\x -> (head x, length x)) (pack str)