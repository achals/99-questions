-- 28.hs Sort a list of lists according to length of lists

-- this is just awesome.

import GHC.Exts

lsort :: [[a]] -> [[a]]
lsort = sortWith (length)