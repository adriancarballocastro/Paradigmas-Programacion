let rec aux x = if x<10 then 1. else 1.+.aux(x/10);;
let rec reverse x = if x<10 then x else (x mod 10)*int_of_float(10.**(aux(x)-.1.)) + reverse(x/10);;

let rec palindromo s = let longitud = String.length s in if longitud <= 1 then true else if s.[0] = s.[longitud - 1] then palindromo (String.sub s 1 (longitud - 2)) else false;;

let rec mcd (x, y) = if y = 0 then x else mcd (y, x mod y);;



