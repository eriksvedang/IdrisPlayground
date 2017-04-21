module Chapter6

-- varargs
AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = Int -> (AdderType k)

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \x => adder k (x + acc)

example : Int
example = adder 4 10 10 10 10 10

-- printf
data Format = Number Format
            | Str Format
            | Lit String Format
            | End
            
PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit _ fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

f : Int
f = ?g 4
