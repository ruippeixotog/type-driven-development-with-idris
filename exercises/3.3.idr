module Chapter3_2

import Data.Vect

Matrix : Nat -> Nat -> Type -> Type
Matrix m n e = Vect m (Vect n e)

transposeMat : Matrix m n a -> Matrix n m a
transposeMat [] = replicate _ []
transposeMat (x :: xs) =
  let transXs = transposeMat xs
      transRow = map (\x => [x]) x in
    zipWith (++) transRow transXs

addMatrix : Num a => Matrix m n a -> Matrix m n a -> Matrix m n a
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = addRow x y :: addMatrix xs ys
  where
    addRow : Vect n a -> Vect n a -> Vect n a
    addRow xs ys = zipWith (+) xs ys

dotProd : Num a => Vect n a -> Vect n a -> a
dotProd xs ys = sum (zipWith (*) xs ys)

multMatrix : Num a => Matrix m n a -> Matrix n p a -> Matrix m p a
multMatrix x y = aux x (transposeMat y)
  where
    aux : Matrix m0 n a -> Matrix p n a -> Matrix m0 p a
    aux [] _ = []
    aux (x :: xs) transY = map (dotProd x) transY :: aux xs transY
