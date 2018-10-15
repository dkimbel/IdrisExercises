data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub
    abs = Abs

Show ty => Show (Expr ty) where
    show (Val x) = show x
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
    show (Abs x) = "(abs " ++ show x ++ ")"

(Eq ty, Neg ty, Integral ty) => Eq (Expr ty) where
    (==) x y = eval x == eval y

(Neg ty, Integral ty) => Cast (Expr ty) ty where
    cast orig = eval orig

-- beginning of section 7.3 exercises
Functor Expr where
    map func (Val x) = Val (func x)
    map func (Add e e') = Add (map func e) (map func e')
    map func (Sub e e') = Sub (map func e) (map func e')
    map func (Mul e e') = Mul (map func e) (map func e')
    map func (Div e e') = Div (map func e) (map func e')
    map func (Abs e) = Abs (map func e)

-- from the book, section 4.2.2 -- I couldn't do it by myself
data MyVect : Nat -> Type -> Type where
     Nil : MyVect Z a
     (::) : (x : a) -> (xs : MyVect k a) -> MyVect (S k) a

-- section 7.3 exercises, continued
Eq a => Eq (MyVect n a) where
    (==) [] [] = True
    (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (MyVect n) where
    foldr func z [] = z
    foldr func z (x :: xs) = func x (foldr func z xs)
