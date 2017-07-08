module Chapter3_1

import Data.Vect

-- exercise 1
myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = (myLength xs) + 1

-- exercise 2
myReverse : List a -> List a
myReverse xs = aux xs []
  where
    aux : List a -> List a -> List a
    aux [] ys = ys
    aux (x :: xs) ys = aux xs (x :: ys)

-- exercise 3
myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

-- exercise 4
myVectMap : (a -> b) -> Vect n a -> Vect n b
myVectMap f [] = []
myVectMap f (x :: xs) = f x :: myVectMap f xs
