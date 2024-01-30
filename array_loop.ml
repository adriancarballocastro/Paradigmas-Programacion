let append arr1 arr2 =
  let len1 = Array.length arr1 in
  let len2 = Array.length arr2 in
  let resultado = Array.make (len1 + len2) arr1.(0) in
  for i = 0 to len1 - 1 do
    resultado.(i) <- arr1.(i);
  done;
  for i = 0 to len2 - 1 do
    resultado.(len1 + i) <- arr2.(i);
  done;
  resultado

let sub arr start len =
  let resultado = Array.make len arr.(0) in
  for i = 0 to len - 1 do
    resultado.(i) <- arr.(start + i);
  done;
  resultado

let copy arr =
  let len = Array.length arr in
  let resultado = Array.make len arr.(0) in
  for i = 0 to len - 1 do
    resultado.(i) <- arr.(i);
  done;
  resultado

let fill arr start len value =
  for i = start to start + len - 1 do
    arr.(i) <- value;
  done

let blit src src_pos dst dst_pos len =
  for i = 0 to len - 1 do
    dst.(dst_pos + i) <- src.(src_pos + i);
  done

let to_list arr =
  let len = Array.length arr in
  let rec loop i acc =
    if i < len then loop (i + 1) (arr.(i) :: acc)
    else List.rev acc
  in
  loop 0 []

let iter f arr =
  let len = Array.length arr in
  for i = 0 to len - 1 do
    f arr.(i)
  done

let fold_left f init arr =
  let len = Array.length arr in
  let resultado = ref init in
  for i = 0 to len - 1 do
    resultado := f !resultado arr.(i)
  done;
  !resultado

let for_all pred arr =
  let len = Array.length arr in
  let rec loop i =
    if i < len then pred arr.(i) && loop (i + 1)
    else true
  in
  loop 0

let exists pred arr =
  let len = Array.length arr in
  let rec loop i =
    if i < len then pred arr.(i) || loop (i + 1)
    else false
  in
  loop 0

let find_opt pred arr =
  let len = Array.length arr in
  let rec loop i =
    if i < len then
      if pred arr.(i) then Some arr.(i)
      else loop (i + 1)
    else None
  in
  loop 0;;


















