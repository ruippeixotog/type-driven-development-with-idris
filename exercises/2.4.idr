module Chapter2_4

-- exercise 2
palindrome : String -> Bool
palindrome str = str == (reverse str)

-- exercise 3
-- palindrome : String -> Bool
-- palindrome str = (toLower str) == (toLower $ reverse str)

-- exercise 4
-- palindrome : String -> Bool
-- palindrome str = length str > 10 &&
--                  (toLower str) == (toLower $ reverse str)

-- exercise 5
-- palindrome : Nat -> String -> Bool
-- palindrome n str = length str > n &&
--                    (toLower str) == (toLower $ reverse str)

-- exercise 6
counts : String -> (Nat, Nat)
counts str = (length $ words str, length str)

-- exercise 7
top_ten : Ord a => List a -> List a
top_ten xs = take 10 $ reverse $ sort xs

-- exercise 8
over_length : Nat -> List String -> Nat
over_length n = length . (filter (\x => length x > n))

-- exercise 9
main : IO ()
main = repl "\nEnter a string: " (show . palindrome)
