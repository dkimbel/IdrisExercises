-- This does indeed break. When you use this naming syntax at type-level,
-- do you have to use a higher-kinded type? Or maybe that isn't the right
-- term; generally, do you need to use a type like Type, that is a type
-- of types? And yet, no; i think we've seen an (x : Bool) case. Yes; indeed
-- we have. I guess the idea is that, AFTER substituting in all of the
-- variables in your type signature, you should have a valid signature.
-- That isn't true in my case below, because `a` is a VALUE of type String,
-- and therefore cannot stand alone by itself as the return type.
shouldBreak : (a : String) -> a
shouldBreak a = a
