import Data.Vect

append_nil : Vect m elem -> Vect (plus m 0) elem
append_nil {m} xs = rewrite plusZeroRightNeutral m in xs

append_xs : Vect (S (m + k)) elem -> Vect (plus m (S k)) elem
append_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs

-- NOTE: as mentioned by the book, this is purely illustrational. This code is
-- much simpler when you instead use (n + m) in the type signature below.
append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append {m} [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)
