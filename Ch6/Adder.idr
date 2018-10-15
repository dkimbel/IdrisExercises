AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = Int -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)

AdderTypePoly : (numargs : Nat) -> Type -> Type
AdderTypePoly Z numType = numType
AdderTypePoly (S k) numType = numType -> AdderTypePoly k numType

adderPoly : Num numType => (numargs : Nat) -> numType -> AdderTypePoly numargs numType
adderPoly Z acc = acc
adderPoly (S k) acc = \next => adderPoly k (next + acc)
