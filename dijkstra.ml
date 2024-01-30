exception Invalid_argument of string

let dijkstra (w : int option array array) : int option array array =
  let n = Array.length w in
  if n = 0 || Array.exists (fun row -> Array.length row <> n) w || Array.exists (fun row -> Array.exists (fun x -> match x with Some weight -> weight < 0 | _ -> false) row) w then
    raise (Invalid_argument "dijkstra")
  else
    let resultado = Array.make_matrix n n None in
    let rec loop visited dist =
      if not (Queue.is_empty dist) then
        let u = Queue.pop dist in
        if not (visited.(u)) then begin
          visited.(u) <- true;
          for v = 0 to n - 1 do
            match w.(u).(v) with
            | Some weight ->
              let alt = match resultado.(u).(v) with Some d -> d + weight | None -> weight in
              (match resultado.(u).(v) with
               | Some d when alt < d -> resultado.(u).(v) <- Some alt
               | None -> resultado.(u).(v) <- Some alt
               | _ -> ());
              if not visited.(v) then Queue.push v dist
            | _ -> ()
          done;
          loop visited dist
        end else
          loop visited dist
    in
    for i = 0 to n - 1 do
      let visited = Array.make n false in
      let dist = Queue.create () in
      Queue.push i dist;
      loop visited dist
    done;
    resultado;;
