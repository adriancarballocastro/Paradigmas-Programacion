let rec power (x: int) (y: int) : int = if y = 0 then 1 else x * power x (y - 1)


let rec power' (x: int) (y: int) : int = if y = 0 then 1
else if y mod 2 = 0 then
    let temp = power' x (y / 2) in
    temp * temp
  else
    x * power' x (y - 1);;
  
    
let rec powerf (x: float) (n: int) : float =
  if n = 0 then 1.0
  else if n mod 2 = 0 then
    let temp = powerf x (n / 2) in
    temp *. temp
  else
    x *. powerf x (n - 1);;

