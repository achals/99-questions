-- 24.hs Lotto: Draw n numbers from a range of 1..M

-- This follows more easily using the solution to 23

import System.Random

diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
            gen <- getStdGen
            return $ take n [x | x <- randomRs(0, m) gen]