-- I was curious to see whether this would work. It doesn't. Indeed,
-- Idris's built-in `the` function really is leveraging dependent types.
-- That comes down to the fact that it assigns its first type argument
-- to a name. As I understand it now, in the actual definition of `the`,
-- (ty : Type) indicates that ty is a VALUE of type Type -- or itself a type,
-- like Bool or String. Thus, when we substitute in that type (Bool, String,
-- whatever) in the rest of the type signature, we get something valid.
fakeThe : a -> a -> a
fakeThe ty x = x
