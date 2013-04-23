-- 25.hs Generate a random permutation of a list

import System.Random
import Data.List

rnd_permu :: [a] -> IO [a]
rnd_permu arr = do
          get <- getStdGen
          index <- randomRIO (0, (length $ permutations arr) - 1)
          return $ permutations arr !! index