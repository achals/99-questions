-- 48.hs
-- Truth table for logical expressions using arbitrary numbers of variables


tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen num func = do
         mapM_ putStrLn [show elem ++ " " ++ show (func elem) | elem <- perms num]


perms :: Int -> [[Bool]]
perms 0 = [[]]
perms num = (map (True :) $ perms (num-1)) ++ (map (False :) $ perms (num-1))

and' :: Bool -> Bool -> Bool
and' x y = and [x, y]

or' :: Bool -> Bool -> Bool
or' x y = or [x, y]

not' :: Bool -> Bool
not' = not

equ' :: Bool -> Bool -> Bool
equ' = (==)

infixl 4 `or'`
infixl 6 `and'`