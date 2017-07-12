module Chapter8_3

data Vect : Nat -> Type -> Type where
  Nil : Vect 0 elem
  (::) : elem -> Vect len elem -> Vect (S len) elem

-- exercise 1
headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
  (contra : (x = y) -> Void) -> (x :: xs = y :: ys) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
  (contra : (xs = ys) -> Void) -> (x :: xs = y :: ys) -> Void
tailUnequal contra Refl = contra Refl

-- exercise 2
DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) =
    case (decEq x y, decEq xs ys) of
      (Yes prf1, Yes prf2) => rewrite prf1 in rewrite prf2 in Yes Refl
      (No contra, _) => No (headUnequal contra)
      (_, No contra) => No (tailUnequal contra)
