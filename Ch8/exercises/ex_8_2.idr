import Data.Vect

-- TODO: The first exercise here is outside of my present capabilities.
-- rewriteSucc : ((k + m) = (m + k)) -> S (plus k m) = plus m (S k)
-- rewriteSucc {k} {m} prf = rewrite sym (plusSuccRightSucc m k) in prf

-- myPlusCommutative : (n : Nat) -> (m : Nat) -> n + m = m + n
-- myPlusCommutative Z m = ?myPlusCommutative_rhs_1
-- myPlusCommutative (S k) m = rewriteSucc (myPlusCommutative k m)

inefficientReverse : List a -> List a
inefficientReverse [] = []
inefficientReverse (x :: xs) = inefficientReverse xs ++ [x]

-- This is a list version of the book's efficient vector reverse.
-- This implementation simply traverses the list it needs to reverse
-- one time, using cons to prepend each successive value onto an
-- accumulating reversed list. By using cons like this, we don't need
-- to use the less efficient `++`.
betterReverse : List a -> List a
betterReverse xs = reverse' [] xs
  where reverse' : List a -> List a -> List a
        reverse' acc [] = acc
        reverse' acc (x :: xs)
                        = reverse' (x :: acc) xs

reverseProof_nil : (acc : Vect k a) -> Vect (plus k 0) a
reverseProof_nil {k} acc = rewrite plusZeroRightNeutral k in acc

-- TODO: I had to get to `sym (plusSuccRightSucc k m)` by trial and error; it was
-- my fourth guess out of four possibilities. I'd like to understand what exactly
-- that function is doing better, so that I can write these proofs correctly
-- with higher confidence.
-- TODO: I also don't understand why this type signature doesn't include a
-- `Vect (S (k + m)) a`, which is more of what I would have expected given the
-- type signature of plusSuccRightSucc.
reverseProof_xs : Vect ((S k) + m) a -> Vect (plus k (S m)) a
reverseProof_xs {k} {m} xs = rewrite sym (plusSuccRightSucc k m) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' acc [] = reverseProof_nil acc
        reverse' acc (x :: xs)
                        = reverseProof_xs (reverse' (x :: acc) xs)
