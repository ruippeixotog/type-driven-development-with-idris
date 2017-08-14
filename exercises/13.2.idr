module Chaprter13_2

import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackCmd () height (S height)
  Pop : StackCmd Integer (S height) height
  Top : StackCmd Integer (S height) (S height)

  GetStr : StackCmd String height height
  PutStr : String -> StackCmd () height height

  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd a height1 height2 ->
          (a -> StackCmd b height2 height3) ->
          StackCmd b height1 height3

runStack : (stk : Vect inHeight Integer) ->
           StackCmd ty inHeight outHeight ->
           IO (ty, Vect outHeight Integer)
runStack stk (Push val) = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk)
runStack (val :: stk) Top = pure (val, val :: stk)
runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr x) = do putStr x
                             pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (x', newStk) <- runStack stk x
                            runStack newStk (f x')

testAdd : StackCmd () 0 0
testAdd = do Push 10
             x <- GetStr
             Push (cast x)
             val1 <- Pop
             val2 <- Pop
             PutStr (show (val1 + val2) ++ "\n")

data StackIO : Nat -> Type where
  Do : StackCmd a height1 height2 ->
       (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackDo
  (>>=) : StackCmd a height1 height2 ->
          (a -> Inf (StackIO height2)) -> StackIO height1
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run (More fuel) stk (Do c f)
     = do (res, newStk) <- runStack stk c
          run fuel newStk (f res)
run Dry stk p = pure ()

doDiscard : StackCmd () (S height) height
doDiscard = do val <- Pop
               PutStr ("Discarded " ++ show val ++ "\n")

doDuplicate : StackCmd () (S height) (S (S height))
doDuplicate = do val <- Pop
                 Push val
                 Push val
                 PutStr ("Duplicated " ++ show val ++ "\n")

doUnOp : (Integer -> Integer) -> StackCmd () (S height) (S height)
doUnOp f = do val <- Pop
              let res = f val
              Push res
              PutStr (show res ++ "\n")

doBinOp : (Integer -> Integer -> Integer) -> StackCmd () (S (S height)) (S height)
doBinOp f = do val1 <- Pop
               val2 <- Pop
               let res = f val2 val1
               Push res
               PutStr (show res ++ "\n")

mutual
  tryDiscard : StackIO height
  tryDiscard {height = S h} = do doDiscard
                                 stackCalc
  tryDiscard = do PutStr "Fewer than one item on the stack\n"
                  stackCalc

  tryDuplicate : StackIO height
  tryDuplicate {height = S h} = do doDuplicate
                                   stackCalc
  tryDuplicate = do PutStr "Fewer than one item on the stack\n"
                    stackCalc

  tryUnOp : (Integer -> Integer) -> StackIO height
  tryUnOp f {height = S h} = do doUnOp f
                                stackCalc
  tryUnOp _ = do PutStr "Fewer than one item on the stack\n"
                 stackCalc

  tryBinOp : (Integer -> Integer -> Integer) -> StackIO height
  tryBinOp f {height = S (S h)} = do doBinOp f
                                     stackCalc
  tryBinOp _ = do PutStr "Fewer than two items on the stack\n"
                  stackCalc

  data StkInput = Number Integer | Add | Subtract | Multiply | Negate | Discard | Duplicate

  strToInput : String -> Maybe StkInput
  strToInput "" = Nothing
  strToInput "add" = Just Add
  strToInput "subtract" = Just Subtract
  strToInput "multiply" = Just Multiply
  strToInput "negate" = Just Negate
  strToInput "discard" = Just Discard
  strToInput "duplicate" = Just Duplicate  
  strToInput x = if all isDigit (unpack x)
                    then Just (Number (cast x))
                    else Nothing

  stackCalc : StackIO height
  stackCalc = do PutStr "> "
                 input <- GetStr
                 case strToInput input of
                      Nothing => do PutStr "Invalid input\n"
                                    stackCalc
                      Just (Number x) => do Push x
                                            stackCalc
                      Just Add => tryBinOp (+)
                      Just Subtract => tryBinOp (-)
                      Just Multiply => tryBinOp (*)
                      Just Negate => tryUnOp (0 -)
                      Just Discard => tryDiscard
                      Just Duplicate => tryDuplicate

main : IO ()
main = run forever [] stackCalc
