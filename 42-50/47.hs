-- 47.hs
-- Truth tables for logical expressions -2
-- Defining operators

-- What? Just infixl?


table' :: (Bool -> Bool -> Bool) -> IO ()
table' func = do
          mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (func a b) | a <- [True, False], b <-[True, False]]

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