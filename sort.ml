let rec insert x = function
[] -> [x]
| h::t -> if x <= h then x :: h :: t
else h :: insert x t


let rec isort = function
[] -> []
| h::t -> insert h (isort t)


let bigl = List.init 1000000 (fun x -> x);;



let insert_t x lst =
  let rec insert_aux acc = function
    | [] -> List.rev (x :: acc)
    | h :: t ->
        if x <= h then List.rev_append acc (x :: h :: t)
        else insert_aux (h :: acc) t
  in
  insert_aux [] lst

let isort_t lst =
  let rec isort_aux acc = function
    | [] -> List.rev acc
    | h :: t -> isort_aux (insert_t h acc) t
  in
  isort_aux [] lst

  
  
  
let rec rlist n =
  if n <= 0 then []
  else Random.int 100 :: rlist (n - 1)



let rec lc1_aux acc n =
  if n = 10000 then List.rev acc
  else lc1_aux (n :: acc) (n + 1)

let lc1 = lc1_aux [] 0

let rec lc2_aux acc n =
  if n = 20000 then List.rev acc
  else lc1_aux (n :: acc) (n + 1)

let lc2 = lc2_aux [] 0


let rec ld1_aux acc n =
  if n = 0 then List.rev acc
  else ld1_aux (n :: acc) (n - 1)

let ld1 = ld1_aux [] 10000

let rec ld2_aux acc n =
  if n = 0 then List.rev acc
  else ld2_aux (n :: acc) (n - 1)

let ld2 = ld2_aux [] 20000
  
  
let lr1 = rlist 10000
let lr2 = rlist 20000














let rec insert x lst cmp =
  match lst with
  | [] -> [x]
  | hd :: tl ->
    if cmp x hd then x :: hd :: tl
    else hd :: insert x tl cmp

let rec isort_g cmp lst =
  let rec isort_aux acc = function
    | [] -> List.rev acc
    | hd :: tl -> isort_aux (insert hd acc cmp) tl
  in
  isort_aux [] lst
  
  
  
  
  
let bigl2 = List.init 1000000 (fun x -> x);;



let rec split_t lst =
  let rec split_acc lst left right =
    match lst with
    | [] -> (List.rev left, List.rev right)
    | x :: xs -> split_acc xs (x :: right) left
  in
  split_acc lst [] []

let rec merge_t (lst1, lst2) =
  let rec merge_acc lst1 lst2 acc =
    match (lst1, lst2) with
    | ([], _) -> List.rev_append acc lst2
    | (_, []) -> List.rev_append acc lst1
    | (x1 :: xs1, x2 :: xs2) ->
      if x1 <= x2 then
        merge_acc xs1 lst2 (x1 :: acc)
      else
        merge_acc lst1 xs2 (x2 :: acc)
  in
  merge_acc lst1 lst2 []


let rec msort' lst =
  match lst with
  | [] -> []
  | [_] -> lst
  | _ ->
    let left, right = split_t lst in
    merge_t (msort' left, msort' right)



let bigl3 = []

let rec msort_g cmp lst =
  let rec split_t lst =
    let rec split_acc lst left right =
      match lst with
      | [] -> (List.rev left, List.rev right)
      | x :: xs -> split_acc xs (x :: right) left
    in
    split_acc lst [] []
  in

  let rec merge_t (lst1, lst2) =
    let rec merge_acc lst1 lst2 acc =
      match (lst1, lst2) with
      | ([], _) -> List.rev_append acc lst2
      | (_, []) -> List.rev_append acc lst1
      | (x1 :: xs1, x2 :: xs2) ->
        if cmp x1 x2 then
          merge_acc xs1 lst2 (x1 :: acc)
        else
          merge_acc lst1 xs2 (x2 :: acc)
    in
    merge_acc lst1 lst2 []
  in

  match lst with
  | [] -> []
  | [_] -> lst
  | _ ->
    let left, right = split_t lst in
    merge_t (msort_g cmp left, msort_g cmp right);;




