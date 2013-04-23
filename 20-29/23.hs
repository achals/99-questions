-- 23.hs Extracintg a number of random elements from a list

-- This took me a disturbingly large amount of time. Had to peek as well :( 

import System.Random
import Data.List

--randomList :: (Random a) => (a, a) -> Int -> StdGen -> [a]
--randomList bnds n = take n . randomRs bnds

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
           gen <- getStdGen
           return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]
