let attack (i1,j1) (i2,j2) = 
    i1 = i2 || 
    j1 = j2 ||
    abs (i2-i1) = abs (j2-j1);;
	
let rec compatible p l = 
    not (List.exists (attack p) l);; 
  
let queens n = 
    let rec search_all_from path (i,j) =
        if i > n then [path]
        else if j > n then []
        else if compatible (i,j) path then
            search_all_from ((i,j)::path) (i+1,1) @ search_all_from path (i,j+1) 
        else search_all_from path (i,j+1)
    in search_all_from [] (1,1);;


let rec not_attacking x y sol =
  match sol with
  | [] -> true
  | (a, b)::rest ->
    y <> b && abs (x - a) <> abs (y - b) && not_attacking x y rest;;

let rec is_queens_sol n sol =
  match sol with
  | [] -> true
  | (x, y)::rest ->
    y <= n && not_attacking x y rest && is_queens_sol n rest;;
