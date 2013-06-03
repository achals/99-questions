--46.hs 
-- Truth tables for expressions

table :: (Bool -> Bool -> Bool) -> IO ()
table func = do
           putStrLn $ show True ++ " " ++ show True ++ " " ++ show (func True True)
           putStrLn $ show True ++ " " ++ show False ++ " " ++ show (func True False)
           putStrLn $ show False ++ " " ++ show True ++ " " ++ show (func False True)
           putStrLn $ show False ++ " " ++ show False ++ " " ++ show (func False False)

-- The solution is much smarter and consise
-- mapM_ = awesome

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