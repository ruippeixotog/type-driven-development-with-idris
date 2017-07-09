module Chapter5_2

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

-- exercise 1
guess : (target : Nat) -> IO ()
guess target = do
  putStr "Insert a number: "
  Just n <- readNumber
    | Nothing => do putStrLn "Invalid number"
                    guess target
  case compare n target of
    LT => do putStrLn "Too low"
             guess target
    GT => do putStrLn "Too high"
             guess target
    EQ => putStrLn "You got it!"

-- exercise 2
main : IO ()
main = do
  target <- time
  guess (cast (mod target 100))

-- exercise 3
guessN : (target : Nat) -> (guesses : Nat) -> IO ()
guessN target Z = putStrLn "Game over."
guessN target (S k) = do
  putStr ("Insert a number (" ++ (show (S k)) ++ " guesses left): ")
  Just n <- readNumber
    | Nothing => do putStrLn "Invalid number"
                    guessN target (S k)
  case compare n target of
    LT => do putStrLn "Too low"
             guessN target k
    GT => do putStrLn "Too high"
             guessN target k
    EQ => putStrLn "You got it!"

mainN : IO ()
mainN = do
  target <- time
  guessN (cast (mod target 100)) 7

-- exercise 4
myRepl : String -> (String -> String) -> IO ()
myRepl prompt onInput = do
  putStr prompt
  input <- getLine
  putStr (onInput input)
  myRepl prompt onInput

myReplWith : (state : a) ->
    (prompt : String) ->
    (onInput : a -> String -> Maybe (String, a)) -> IO ()

myReplWith state prompt onInput = do
  putStr prompt
  input <- getLine
  case (onInput state input) of
    Nothing => pure ()
    Just (output, newState) => do
      putStr output
      myReplWith newState prompt onInput
