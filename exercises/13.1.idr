module Chapter13_1

data DoorState = DoorOpen | DoorClosed

-- exercise 1
namespace Door
  data DoorCmd : Type -> DoorState -> DoorState -> Type where
    Open : DoorCmd () DoorClosed DoorOpen
    Close : DoorCmd () DoorOpen DoorClosed
    RingBell : DoorCmd () st st

    Pure : ty -> DoorCmd ty st st
    (>>=) : DoorCmd a st1 st2 -> (a -> DoorCmd b st2 st3) -> DoorCmd b st1 st3

doorProg : DoorCmd () DoorClosed DoorClosed
doorProg = do RingBell
              Open
              RingBell
              Close

-- exercise 2
namespace Guess
  data GuessCmd : Type -> Nat -> Nat -> Type where
    Try : Integer -> GuessCmd Ordering (S st) st

    Pure : ty -> GuessCmd ty st st
    (>>=) : GuessCmd a st1 st2 -> (a -> GuessCmd b st2 st3) -> GuessCmd b st1 st3

threeGuesses : GuessCmd () 3 0
threeGuesses = do Try 10
                  Try 20
                  Try 15
                  Pure ()

-- noGuesses : GuessCmd () 0 0
-- noGuesses = do Try 10
--                Pure ()

-- exercise 3
data Matter = Solid | Liquid | Gas

namespace Matter
  data MatterCmd : Type -> Matter -> Matter -> Type where
    Melt : MatterCmd () Solid Liquid
    Boil : MatterCmd () Liquid Gas
    Condense : MatterCmd () Gas Liquid
    Freeze : MatterCmd () Liquid Solid

    Pure : ty -> MatterCmd ty st st
    (>>=) : MatterCmd a st1 st2 -> (a -> MatterCmd b st2 st3) -> MatterCmd b st1 st3

iceSteam : MatterCmd () Solid Gas
iceSteam = do Melt
              Boil

steamIce : MatterCmd () Gas Solid
steamIce = do Condense
              Freeze

-- overMelt : MatterCmd () Solid Gas
-- overMelt = do Melt
--               Melt
