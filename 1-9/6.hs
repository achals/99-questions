-- Check if a list is a palindrome or not.
-- Interesting point : the elements must be comparable!
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (xs) = (head (xs)) == (last (xs)) && isPalindrome (init (tail(xs)) )
