let rec sumto = function 0 -> 0 | n -> n + sumto (n-1);;
let rec exp10 = function 0 -> 10 | n -> 10 * exp10 (n-1);;
let rec num_cifras = function 0 ->0| n -> 1 + num_cifras (n/10);;
let rec sum_cifras = function 0 -> 0| n -> abs n mod 10 + sum_cifras (abs n/10);;
