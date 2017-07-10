module Chapter7_2

data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

-- exercise 1
Show num => Show (Expr num) where
  show (Val x) = show x
  show (Add x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Sub x y) = "(" ++ (show x) ++ " - " ++ (show y) ++ ")"
  show (Mul x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
  show (Div x y) = "(" ++ (show x) ++ " / " ++ (show y) ++ ")"
  show (Abs x) = "|" ++ (show x) ++ "|"

-- exercise 2
(Eq num, Neg num, Integral num) => Eq (Expr num) where
  (==) a b = eval a == eval b

-- exercise 3
(Neg num, Integral num) => Cast (Expr num) num where
  cast orig = eval orig
