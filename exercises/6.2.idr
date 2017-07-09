module Chapter6_2

import Data.Vect

-- exercise 1
Matrix : Nat -> Nat -> Type
Matrix m n = Vect m (Vect n Double)

-- exercise 2
data Format = Number Format
            | Str Format
            | Ch Format
            | Dbl Format
            | Lit String Format
            | End

toFormat : List Char -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: rest) = Number (toFormat rest)
toFormat ('%' :: 's' :: rest) = Str (toFormat rest)
toFormat ('%' :: 'c' :: rest) = Ch (toFormat rest)
toFormat ('%' :: 'f' :: rest) = Dbl (toFormat rest)
toFormat (ch :: rest) = case toFormat rest of
                          Lit str fmt => Lit (strCons ch str) fmt
                          fmt => Lit (cast ch) fmt

PrintfType : Format -> Type
PrintfType (Number fmt) = Int -> PrintfType fmt
PrintfType (Str fmt) = String -> PrintfType fmt
PrintfType (Ch fmt) = Char -> PrintfType fmt
PrintfType (Dbl fmt) = Double -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \k => printfFmt fmt (acc ++ (show k))
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Ch fmt) acc = \ch => printfFmt fmt (acc ++ (show ch))
printfFmt (Dbl fmt) acc = \x => printfFmt fmt (acc ++ (show x))
printfFmt (Lit str fmt) acc = printfFmt fmt (acc ++ str)
printfFmt End acc = acc

printf : (fmtStr : String) -> PrintfType (toFormat (cast fmtStr))
printf fmtStr = printfFmt _ ""

-- exercise 3
TupleVect : Nat -> Type -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

test : TupleVect 4 Nat
test = (1,2,3,4,())
