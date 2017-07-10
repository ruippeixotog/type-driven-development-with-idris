module Chapter7_3

data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

-- exercise 1
Functor Expr where
  map f (Val x) = Val (f x)
  map f (Add x y) = Add (map f x) (map f y)
  map f (Sub x y) = Sub (map f x) (map f y)
  map f (Mul x y) = Mul (map f x) (map f y)
  map f (Div x y) = Div (map f x) (map f y)
  map f (Abs x) = Abs (map f x)

-- exercise 2
data Vect : Nat -> Type -> Type where
  Nil : Vect 0 elem
  (::) : elem -> Vect len elem -> Vect (S len) elem

Eq elem => Eq (Vect n elem) where
  (==) [] [] = True
  (==) (x :: xs) (x' :: xs') = x == x' && xs == xs'

Foldable (Vect n) where
  foldr func acc [] = acc
  foldr func acc (x :: xs) = func x (foldr func acc xs)
