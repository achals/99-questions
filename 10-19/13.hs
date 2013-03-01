-- 13.hs Direct run length encoding
-- Without making temporary lists containing duplicates as in 9.hs

data Encoding a = Single a
                | Multiple Int a
                deriving (Show, Eq)

encodeOne :: [a] -> Encoding a
encodeOne list 
          | (length list == 1) = Single (head list)
          | otherwise          = Multiple (length list) (head list)

encodeDirect :: Eq a => [a] -> [Encoding a] 
encodeDirect (x:xs) = encodedFirst : encodeDirect rest
                    where
                    (first, rest) = span (== x) (x:xs)
                    encodedFirst = encodeOne first