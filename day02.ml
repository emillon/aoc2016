module Dir = struct
  type t =
    | U
    | D
    | L
    | R

  let parse = function
    | 'U' -> U
    | 'D' -> D
    | 'L' -> L
    | 'R' -> R
    | c -> raise_s [%message (c : char)]
  ;;

  let vec = function
    | U -> 0, 1
    | D -> 0, -1
    | L -> -1, 0
    | R -> 1, 0
  ;;
end

module Digit = struct
  let of_pos1 = function
    | (2 | -2), _ -> None
    | _, (2 | -2) -> None
    | -1, 1 -> Some '1'
    | 0, 1 -> Some '2'
    | 1, 1 -> Some '3'
    | -1, 0 -> Some '4'
    | 0, 0 -> Some '5'
    | 1, 0 -> Some '6'
    | -1, -1 -> Some '7'
    | 0, -1 -> Some '8'
    | 1, -1 -> Some '9'
    | p -> raise_s [%message (p : Vec.t)]
  ;;

  let of_pos2 = function
    | p when Vec.l1_norm p > 2 -> None
    | 0, 2 -> Some '1'
    | -1, 1 -> Some '2'
    | 0, 1 -> Some '3'
    | 1, 1 -> Some '4'
    | -2, 0 -> Some '5'
    | -1, 0 -> Some '6'
    | 0, 0 -> Some '7'
    | 1, 0 -> Some '8'
    | 2, 0 -> Some '9'
    | -1, -1 -> Some 'A'
    | 0, -1 -> Some 'B'
    | 1, -1 -> Some 'C'
    | 0, -2 -> Some 'D'
    | p -> raise_s [%message (p : Vec.t)]
  ;;
end

let get_code to_pos l =
  List.fold l ~init:Vec.zero ~f:(fun pos dir ->
    let new_pos = Vec.add pos (Dir.vec dir) in
    match to_pos new_pos with
    | Some _ -> new_pos
    | None -> pos)
  |> to_pos
  |> Option.value_exn
;;

let of_digits = String.of_char_list
let parse_line s = String.to_list s |> List.map ~f:Dir.parse
let parse s = String.split_lines s |> List.map ~f:parse_line
let f1 s = parse s |> List.map ~f:(fun l -> get_code Digit.of_pos1 l) |> of_digits
let f2 s = parse s |> List.map ~f:(fun l -> get_code Digit.of_pos2 l) |> of_digits
let run () = Run.run_string ~name:"day02" ~f1 ~f2 Day02_input.data
