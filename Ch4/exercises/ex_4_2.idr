import Data.Vect

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Unicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     ElectricCar : (charge : Nat) -> Vehicle Electric

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Motorcycle fuel) = 2
wheels (ElectricCar charge) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 40
refuel Bicycle impossible
refuel Unicycle impossible
refuel (ElectricCar charge) impossible

-- I unfortunately did not come up with this type signature on my own; rather,
-- I needed to refer to Idris's Vect.take signature
vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z [] = []
vectTake Z (x :: xs) = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n = Z} pos [] [] = Nothing
sumEntries {n = S k} pos xs ys = case integerToFin pos (S k) of
                                     Nothing => Nothing
                                     (Just i) => Just (Vect.index i xs + Vect.index i ys)

total myIndex : Fin n -> Vect n a -> a
myIndex (FZ) (x :: _) = x
myIndex (FS k) (_ :: xs) = myIndex k xs
