let list_add (lt: (int*int) list): (int list) =
  List.map (fun (a, b) -> a + b) lt
;;

let list_div (lt: (int*int) list): (int option list) =
  List.map (fun (a,b) -> if b = 0 then None else Some (a / b)) lt
;;

let rec filter_none (lt: int option list): int list =
  match lt with
  | [] -> []
  | x :: lt' ->
      match x with
      | None -> filter_none lt'
      | Some p -> p :: filter_none lt'
;;

let rec sum_squares (lt: int list): int =
  let sum x y = x * x + y in
  List.fold_right sum lt 0
;;
