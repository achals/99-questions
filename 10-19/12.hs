--12.hs, modified decode.
-- Using the same data type from 11.hs

data Encoding a = Single a
     	      | Multiple Int a
              deriving (Show, Eq)


fromEncoding :: Encoding a ->[a]
fromEncoding (Single a) = [a]
fromEncoding (Multiple num a) = replicate num a

decodeModified :: [Encoding a] -> [a]
decodeModified list = concat (map fromEncoding list)