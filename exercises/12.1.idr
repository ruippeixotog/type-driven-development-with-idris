import Control.Monad.State

data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

-- exercise 1
update : (stateType -> stateType) -> State stateType ()
update f = do state <- get
              put (f state)

increase : Nat -> State Nat ()
increase inc = do current <- get
                  put (current + inc)

-- exercise 2
countEmpty : Tree a -> State Nat ()
countEmpty Empty = do current <- get
                      put (S current)
countEmpty (Node left val right) = do countEmpty left
                                      countEmpty right

-- exercise 3
countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do (ec, nc) <- get
                          put (S ec, nc)
countEmptyNode (Node left val right) = do (ec, nc) <- get
                                          put (ec, S nc)
                                          countEmptyNode left
                                          countEmptyNode right
