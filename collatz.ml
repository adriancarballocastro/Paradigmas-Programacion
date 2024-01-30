let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1

let rec orbit n =
  if n = 1 then "1"
  else
    let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
    string_of_int n ^ ", " ^ orbit next;;


let rec length n =
  if n = 1 then 0
  else if n mod 2 = 0 then 1 + length (n / 2)
  else 1 + length (3 * n + 1);;


let rec top n =
  let rec find_max n max =
    if n = 1 then max
    else
      let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
      let new_max = if next > max then next else max in
      find_max next new_max
  in
  find_max n n;;


let length'n'top n =
  let rec length'n'top_aux k max_length max_height =
    if k = 1 then (max_length, max_height)
    else if k mod 2 = 0 then
      length'n'top_aux (k / 2) (max_length + 1) (max max_height k)
    else
      length'n'top_aux (3 * k + 1) (max_length + 1) (max max_height k)
  in
  length'n'top_aux n 0 n;;


let rec longest_in m n =
  let rec collatz_length num =
    if num = 1 then 1
    else if num mod 2 = 0 then 1 + collatz_length (num / 2)
    else 1 + collatz_length (3 * num + 1)
  in
  let rec find_longest_in_range min_num max_num max_length current_num =
    if min_num > max_num then
      (current_num, max_length - 1)
    else
      let length = collatz_length min_num in
      if length > max_length then
        find_longest_in_range (min_num + 1) max_num length min_num
      else
        find_longest_in_range (min_num + 1) max_num max_length current_num
  in
  find_longest_in_range m n 0 0;;



let rec highest_in m n = 
let rec top n =
  let rec find_max n max =
    if n = 1 then max
    else
      let next = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
      let new_max = if next > max then next else max in
      find_max next new_max in find_max n n
        
  in
  let rec find_high_in_range min_num max_num max_high current_num =
    if min_num > max_num then
      (current_num, max_high)
    else
      let high = top min_num in
      if high > max_high then
        find_high_in_range (min_num + 1) max_num high min_num
      else
        find_high_in_range (min_num + 1) max_num max_high current_num
  in
  find_high_in_range m n 0 0;;












