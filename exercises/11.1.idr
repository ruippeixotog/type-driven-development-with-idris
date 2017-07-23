module Chaprter11_1

import Data.Primitives.Views

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)

getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                 (seed' `shiftR` 2) :: randoms seed'

-- exercise 1
every_other : Stream a -> Stream a
every_other (x1 :: x2 :: xs) = x2 :: every_other xs

-- exercise 2
Functor InfList where
  map f (value :: xs) = f value :: map f xs

-- exercise 3
data Face = Heads | Tails

getFace : Int -> Face
getFace k with (divides k 2)
  getFace (2 * _ + 0) | DivBy _ = Heads
  getFace _ | DivBy _ = Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z _ = []
coinFlips (S k) (rnd :: rnds) = getFace rnd :: coinFlips k rnds

-- exercise 4
square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx =
  approx :: square_root_approx number ((approx + (number / approx)) / 2)

-- exercise 5
square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (val :: _) = val
square_root_bound (S k) number bound (val :: vals) =
  if abs (val * val - number) < bound
    then val
    else square_root_bound k number bound vals

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001
                       (square_root_approx number number)
