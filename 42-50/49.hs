-- 49.hs
-- Gray codes. See: https://en.wikipedia.org/wiki/Gray_code

gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n
     | n < 1 = undefined
     | otherwise = (map ('0' : ) $ gray (n-1)) ++ (map ('1' : ) $ reverse $ gray (n-1))