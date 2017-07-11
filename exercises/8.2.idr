module Chapter8_2

import Data.Vect

-- exercise 1
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in plusSuccRightSucc m k

-- exercise 2
myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where
    reverseProofNil : Vect n a -> Vect (plus n 0) a
    reverseProofNil {n} xs = rewrite plusZeroRightNeutral n in xs

    reverseProofCons : Vect (S n + m) a -> Vect (n + S m) a
    reverseProofCons {n} {m} xs = rewrite sym (plusSuccRightSucc n m) in xs

    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' {n} acc [] = reverseProofNil acc
    reverse' {n} {m = S m'} acc (x :: xs) = reverseProofCons (reverse' (x :: acc) xs)
