total twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible

valueNotSuc : (k : Nat) -> k = S k -> Void
valueNotSuc _ Refl impossible
