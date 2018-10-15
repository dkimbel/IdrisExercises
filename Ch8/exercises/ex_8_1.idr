total same_cons : {xs : List a} -> {ys: List a} -> xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf

-- TODO: check for total after implementing
-- TODO: I wasn't able to complete this exercise at all
same_lists : {xs : List a} -> {ys: List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
-- same_lists prf prf1 = cong {f = (cong {f = \el, ls => el :: ls} prf)} prf1

data ThreeEq : a -> b -> c -> Type where
     Same : (val : a) -> ThreeEq val val val

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z (Same z) = Same (S z)
