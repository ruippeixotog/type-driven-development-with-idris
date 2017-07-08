module Chapter4_2

import Data.List
import Data.Vect

-- exercises 1-2
data PowerSource = Petrol | Battery | Pedal

data Vehicle : PowerSource -> Type where
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  ElectricCar : (power : Nat) -> Vehicle Battery

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels Motorcycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (ElectricCar power) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

-- exercises 3-4
vectTake : (k : Fin len) -> Vect len a -> Vect (finToNat k) a
vectTake FZ xs = []
vectTake (FS k1) (x :: xs) = x :: vectTake k1 xs

-- exercise 5
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = map sumIndex (integerToFin pos n)
  where
    sumIndex : Fin n -> a
    sumIndex k = index k xs + index k ys
