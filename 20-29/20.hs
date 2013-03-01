--20.hs Delete the K'th element from a list

removeAt :: Int -> [a] -> (a, [a])
removeAt idx list = (list !! (idx-1), (take (idx-1) list) ++ (drop idx list))