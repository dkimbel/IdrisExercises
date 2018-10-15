data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
     Same : (num : Nat) -> EqNat num num

-- sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
-- sameS k k (Same k) = Same (S k)

total checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same 0)
checkEqNat Z (S j) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = do Same k <- checkEqNat k j
                            Just (Same (S k))
-- checkEqNat (S k) (S j) = do eq <- checkEqNat k j
--                             Just (sameS _ _ eq)
-- checkEqNat (S k) (S j) = case checkEqNat k j of
--                               Nothing => Nothing
--                               Just (Same j) => Just (Same (S j))
-- checkEqNat (S k) (S j) = case checkEqNat k j of
--                               Nothing => Nothing
--                               Just eq => Just (sameS _ _ eq)

total checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat' Z Z = Just Refl
checkEqNat' Z (S j) = Nothing
checkEqNat' (S k) Z = Nothing
checkEqNat' (S k) (S j) = case checkEqNat' k j of
                               Nothing => Nothing
                               Just prf => Just (cong prf)


total exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = do Same _ <- checkEqNat m len
-- I don't know why, but I can't get the line below to work.
-- exactLength {m} len input = do _ <- checkEqNat' m len
                               pure input
