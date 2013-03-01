-- 11.hs. Modified Run length encoding
-- Using 10.hs 

data Encoding a = Single a
              | Multiple Int a
              deriving (Show, Eq)

pack:: Eq a=> [a]->[[a]]
pack []=[]
pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest

encode :: Eq a => [a] -> [(a, Int)]
encode str = map (\x -> (head x, length x)) (pack str)

toEncode' :: (a, Int) -> Encoding a
toEncode' (x, 1) = Single x
toEncode' (x, num) = Multiple num x

encode' :: Eq a => [a] -> [Encoding a]
encode' list = map toEncode' (encode list)