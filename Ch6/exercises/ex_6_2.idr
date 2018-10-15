import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix rows cols = Vect rows (Vect cols Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

TupleVect : (numargs : Nat) -> (itemtype : Type) -> Type
TupleVect Z _ = ()
TupleVect (S k) itemtype = (itemtype, TupleVect k itemtype)

test : TupleVect 4 Nat
test = (1,2,3,4,())

data Format = Number Format
            | Str Format
            | Chr Format
            | Doub Format
            | Lit String Format
            | End

%name Format fmt

PrintfType : Format -> Type
PrintfType (Number fmt) = Int -> PrintfType fmt
PrintfType (Str fmt) = String -> PrintfType fmt
PrintfType (Chr fmt) = Char -> PrintfType fmt
PrintfType (Doub fmt) = Double -> PrintfType fmt
PrintfType (Lit x fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Chr fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (Doub fmt) acc = \f => printfFmt fmt (acc ++ show f)
printfFmt (Lit x fmt) acc = printfFmt fmt (acc ++ x)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Doub (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars) -- I don't understand why this line is needed
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
