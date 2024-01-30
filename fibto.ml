let rec fibto n =
  let rec fib a b n =
    if a > n then ()
    else
      let _ = Printf.printf "%d\n" a in
      fib b (a + b) n  in fib 0 1 n;;
