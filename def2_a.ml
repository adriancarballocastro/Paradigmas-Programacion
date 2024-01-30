let p n = n *. 2. *. asin(1.) *. 2.;;
let area n = n**2. *. 2. *. asin(1.);;
let absf x = if x < 0.0 then -.x else x ;;
let even x = if x mod 2 == 0 then true else false;;
let next3 x = if x mod 3 = 0 then x else x + (3 - (x mod 3 ));;
let is_a_letter x = if 96 < int_of_char x && 123> int_of_char x then true else false;;
let string_of_bool x = if x = true then "true" else "false";;
