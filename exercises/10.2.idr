module Chapter10_2

import Data.List.Views
import Data.Nat.Views
import Data.Vect
import Data.Vect.Views

-- exercise 1
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix xs ys | xSnoc with (snocList ys)
    equalSuffix [] _ | Empty | _ = []
    equalSuffix _ [] | _ | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | Snoc xRec | Snoc yRec =
      if x == y then equalSuffix xs ys | xRec | yRec else []

-- exercise 2
mergeSort : Ord a => Vect len a -> Vect len a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (ys ++ zs) | SplitRecPair lrec rrec =
    merge (mergeSort ys | lrec) (mergeSort zs | rrec)

-- exercise 3
toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | HalfRecEven rec = (toBinary n | rec) ++ "0"
  toBinary (S (n + n)) | HalfRecOdd rec = (toBinary n | rec) ++ "1"

-- exercise 4
palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | VCons rec =
    x == y && palindrome ys | rec
