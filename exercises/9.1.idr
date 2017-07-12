module Chapter9_1

-- exercise 1
data Elem : (elem : a) -> (xs : List a) -> Type where
  Here : Elem e (e :: xs)
  There : Elem e xs -> Elem e (x :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInCons : (value = x -> Void) -> (Elem value xs -> Void) -> Elem value (x :: xs) -> Void
notInCons notHere notThere Here = notHere Refl
notInCons notHere notThere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : List a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs) =
  case decEq value x of
    Yes Refl => Yes Here
    No notHere => case isElem value xs of
      Yes prf => Yes (There prf)
      No notThere => No (notInCons notHere notThere)

-- exercise 2
data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

notLastInNil : Last [] value -> Void
notLastInNil LastOne impossible
notLastInNil (LastCons _) impossible

notLastInNeq : (x = value -> Void) -> Last [x] value -> Void
notLastInNeq notEq LastOne = notEq Refl
notLastInNeq _ (LastCons LastOne) impossible
notLastInNeq _ (LastCons (LastCons _)) impossible

notLastInTail : (Last (x2 :: xs) value -> Void) -> Last (x :: x2 :: xs) value -> Void
notLastInTail notLast (LastCons later) = notLast later

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notLastInNil
isLast [x] value = case decEq x value of
                     Yes Refl => Yes LastOne
                     No notEq => No (notLastInNeq notEq)
isLast (x :: x2 :: xs) value = case isLast (x2 :: xs) value of
                                 Yes prf => Yes (LastCons prf)
                                 No notLast => No (notLastInTail notLast)
