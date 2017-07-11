module Chapter8_1

-- exercise 1
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons = cong

-- exercise 2
same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl = same_cons

-- exercise 3
data ThreeEq : a -> b -> c -> Type where
  Refl : ThreeEq a a a

-- exercise 4
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS a a a Refl = Refl
