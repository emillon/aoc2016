type tile =
  | Safe
  | Trap
[@@deriving sexp]

let is_safe = function
  | Safe -> true
  | Trap -> false
;;

let tile_of_char = function
  | '.' -> Safe
  | '^' -> Trap
  | c -> raise_s [%message "tile_of_char" (c : char)]
;;

let next_row a ~into =
  Array.iteri a ~f:(fun i center ->
    let left = if i = 0 then Safe else a.(i - 1) in
    let right = if i = Array.length a - 1 then Safe else a.(i + 1) in
    let v =
      match left, center, right with
      | Trap, Trap, Safe -> Trap
      | Safe, Trap, Trap -> Trap
      | Trap, Safe, Safe -> Trap
      | Safe, Safe, Trap -> Trap
      | _ -> Safe
    in
    into.(i) <- v)
;;

let safe_in_row row = Array.count row ~f:is_safe

let count_build_map row ~n =
  let total = ref 0 in
  let a1 = Array.copy row in
  let a2 = Array.create ~len:(Array.length row) Safe in
  let arrays = ref (a1, a2) in
  for _ = 1 to n do
    let cur, next = !arrays in
    total := !total + safe_in_row cur;
    next_row cur ~into:next;
    arrays := next, cur
  done;
  !total
;;

let parse s =
  String.strip s |> String.to_list |> List.map ~f:tile_of_char |> List.to_array
;;

let f_gen s ~n = count_build_map (parse s) ~n
let f1 = f_gen ~n:40
let f2 = f_gen ~n:400000
let run () = Run.run ~f1 ~f2 Day18_input.data
