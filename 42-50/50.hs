--50 .hs
--  Huffman codes. See: https://en.wikipedia.org/wiki/Huffman_coding

{-- Previous attempt made too long ago. Abandoning in favor of different approach.
huffman :: [(Char, Int)] -> [(Char, String)]
huffman entries = result
        where
        result = map (\(elem, count, symbol) -> (elem, symbol)) (foldr build [] entries)
        accCount [] = 0
        accCount ((_, cnt, _):xs) = cnt 
        build :: (Char, Int) -> [(Char, Int, String)] -> [(Char, Int, String)]
        build (elem, freq) acc
              | freq >= accCount acc = (elem, (freq + accCount acc), "0") : map (\(x, y, symbol) -> (x, y, '1': symbol)) acc
              | otherwise = (elem, (freq + accCount acc), "1") : map (\(x, y, symbol) -> (x, y, '0': symbol)) acc
                   

termSum :: [(Char, Int, String)] -> Int
termSum = foldr (\(_, count, _) temp -> count + temp) 0 
--}

-- This solution from the solutions page. A Tree is the best data structure to use.
-- Should have struck me earlier.

import Data.List
import Data.Ord (comparing)

data Tree a = Leaf a | Branch (Tree a) (Tree a)
     deriving Show

huffman :: [(Char, Int)] -> [(Char, String)]
huffman freq = sortBy (comparing fst) $ serialize $
        htree $ sortBy (comparing fst) $ [(w, Leaf x) | (x,w) <- freq]
  where htree [(_, t)] = t
        htree ((w1,t1):(w2,t2):wts) =
                htree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts
        serialize (Branch l r) =
                [(x, '0':code) | (x, code) <- serialize l] ++
                [(x, '1':code) | (x, code) <- serialize r]
        serialize (Leaf x) = [(x, "")]