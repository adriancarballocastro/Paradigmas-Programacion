let min (x: 'a) (y: 'a) : 'a = if x < y then x else y;;
let max (x: 'a) (y: 'a) : 'a = if x > y then x else y;;
let fst (x: 'a * 'b) : 'a = match x with | (a, _) -> a;;
let snd (x: 'a * 'b) : 'b = match x with | (_, b) -> b;;

