let hd = function [] -> failwith "Lista vacia" | x :: _ -> x;;

let rec tl lista = match lista with | [] -> [] | _::rest -> rest;;

let rec length = function [] -> 0 | _:: t -> 1 + length t;;

let rec compare_lengths l1 l2 = match (l1, l2) with ([], []) -> 0 | ([], _) -> -1 | (_, []) -> 1 | (_::t1, _::t2) -> compare_lengths t1 t2;;

let rec compare_length_with l1 l2 = if length l1 = l2 then 0 else if length l1 > l2 then -1 else 1;;

let rec init n f = if n <= 0 then [] else let rest = init (n - 1) f in f (n - 1) :: rest;; 

let rec nth lista n =
  match (lista, n) with
  | ([], _) -> failwith "Lista vacia"
  | (hd :: _, 0) -> hd
  | (_ :: tl, _) when n < 0 -> failwith "Indice negativo"
  | (_ :: tl, n) -> nth tl (n - 1);;

let rec append l1 l2 =
	match l1 with [] -> l2 
	| h :: t -> h :: append t l2;;

let rec rev_append l1 l2 = match l1 with [] -> l2 | h::t -> h :: rev_append t l2;;


let rec rev lista = 
  let rec rev_aux acumulador = function
    | [] -> acumulador
    | cabeza :: resto -> rev_aux (cabeza :: acumulador) resto 
    in rev_aux [] lista;;

let rec concat lista =
  match lista with
  | [] -> []
  | hd::tl -> hd @ concat tl


let rec flatten : 'a list list -> 'a list = function
  | [] -> []
  | hd :: tl -> hd @ flatten tl;;


let rec split lista =
  let rec split_helper lista acc1 acc2 =
    match lista with
    | [] -> (List.rev acc1, List.rev acc2)
    | (x, y)::tl -> split_helper tl (x::acc1) (y::acc2)
  in
  split_helper lista [] [];;


let rec combine list1 list2 =
  match list1, list2 with
  | [], [] -> []
  | x1::rest1, x2::rest2 -> (x1, x2) :: combine rest1 rest2
  | _, _ -> failwith "Listas de diferentes tamaños";;




let rec map f lista =
  match lista with
  | [] -> []
  | hd::tl -> (f hd) :: map f tl;;


let rec map2 f list1 list2 =
  match (list1, list2) with
  | ([], []) -> []
  | (hd1 :: tl1, hd2 :: tl2) -> f hd1 hd2 :: map2 f tl1 tl2
  | _ -> failwith "Listas de diferentes tamaños"


let rec rev_map f lista =
  let rec rev_map_helper f lst acc =
    match lista with
    | [] -> acc
    | hd::tl -> rev_map_helper f tl ((f hd)::acc)
  in
  rev_map_helper f lista []

let rec for_all f lista =
  match lista with
  | [] -> true
  | x :: xs -> if f x then for_all f xs else false

let rec exists f lista =
  match lista with
  | [] -> false
  | x :: xs -> if f x then true else exists f xs

let rec mem x lista =
  match lista with
  | [] -> false
  | y :: ys -> if x = y then true else mem x ys

let rec find f lista =
  match lista with
  | [] -> raise Not_found
  | x :: xs -> if f x then x else find f xs

let rec filter f lista =
  match lista with
  | [] -> []
  | x :: xs -> if f x then x :: filter f xs else filter f xs

let rec find_all f lista =
  match lista with
  | [] -> []
  | x :: xs -> if f x then x :: find_all f xs else find_all f xs

let rec partition f lista =
  match lista with
  | [] -> ([], [])
  | x :: xs ->
      let (left, right) = partition f xs in
      if f x then (x :: left, right) else (left, x :: right)

let rec fold_left f acc lista =
  match lista with
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs

let rec fold_right f lista acc =
  match lista with
  | [] -> acc
  | x :: xs -> f x (fold_right f xs acc)

let rec assoc x lista =
  match lista with
  | [] -> raise Not_found
  | (a, b) :: rest -> if a = x then b else assoc x rest

let rec mem_assoc x lista =
  match lista with
  | [] -> false
  | (a, _) :: rest -> if a = x then true else mem_assoc x rest

let rec remove_assoc x lista =
  match lista with
  | [] -> []
  | (a, b) :: rest -> if a = x then remove_assoc x rest else (a, b) :: remove_assoc x rest;;


