module Chapter5_1

-- exercise 1
printLonger : IO ()
printLonger = do str1 <- getLine
                 str2 <- getLine
                 let len1 = length str1
                 let len2 = length str2
                 putStrLn (show (max len1 len2))

-- exercise 2
printLonger2 : IO ()
printLonger2 = getLine >>= \str1 =>
               getLine >>= \str2 =>
               let len1 = length str1
                   len2 = length str2 in
                 putStrLn (show (max len1 len2))
