module Chapter11_3

import Data.Primitives.Views
import System

%default total

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String
  ReadFile : String -> Command (Maybe String)
  WriteFile : String -> String -> Command (Maybe ())
  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile file) = do Right x <- readFile file
                                  | Left err => pure Nothing
                                pure (Just x)
runCommand (WriteFile file x) = do Right _ <- writeFile file x
                                     | Left err => pure Nothing
                                   pure (Just ())
runCommand (Pure val) = pure val
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)


---

data Input = Answer Int | QuitCmd

State : Type
State = (Nat, Nat)

showState : State -> String
showState (score, cnt) = show score ++ " / " ++ show cnt

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure (Answer (cast answer))

mutual
  correct : Stream Int -> State -> ConsoleIO State
  correct nums (score, cnt)
          = do PutStr "Correct!\n"
               quiz nums (score + 1, cnt + 1)

  wrong : Stream Int -> Int -> State -> ConsoleIO State
  wrong nums ans (score, cnt)
        = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
             quiz nums (score, cnt + 1)

  quiz : Stream Int -> State -> ConsoleIO State
  quiz (num1 :: num2 :: nums) state
    = do PutStr ("Score so far: " ++ showState state ++ "\n")
         input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
         case input of
              Answer answer => if answer == num1 * num2
                                  then correct nums state
                                  else wrong nums (num1 * num2) state
              QuitCmd => Quit state

---

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                 (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound x with (divides x 12)
      bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

---

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)
run Dry p = pure Nothing

partial
main : IO ()
main = do seed <- time
          Just state <- run forever (quiz (arithInputs (fromInteger seed)) (0, 0))
              | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ showState state)

--
-- exercise 3
data ShellInput = Cat String | Copy String String | Exit

readShellInput : (prompt : String) -> Command (Maybe ShellInput)
readShellInput prompt = do PutStr prompt
                           answer <- GetLine
                           case (words answer) of
                             "cat" :: file :: [] => Pure (Just (Cat file))
                             "copy" :: src :: dst :: [] => Pure (Just (Copy src dst))
                             "exit" :: [] => Pure (Just Exit)
                             _ => Pure Nothing

shell : ConsoleIO ()
shell = do Just input <- readShellInput "> "
             | Nothing => do PutStr "Illegal command\n"
                             shell
           case input of
             Cat file => shellCat file
             Copy src dst => shellCopy src dst
             Exit => Quit ()
  where
    shellCat : String -> ConsoleIO ()
    shellCat file = do res <- ReadFile file
                       case res of
                         Just content => PutStr content
                         Nothing => PutStr "Cannot read file\n"
                       shell
    shellCopy : String -> String -> ConsoleIO ()
    shellCopy src dst = do Just content <- ReadFile src
                             | Nothing => do PutStr "Cannot read file\n"
                                             shell
                           Just _ <- WriteFile dst content
                             | Nothing => do PutStr "Cannot write file\n"
                                             shell
                           shell

partial
shellMain : IO ()
shellMain = do Just _ <- run forever shell
                 | Nothing => putStrLn "Ran out of fuel"
               pure ()
