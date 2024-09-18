let parse_line =
  let open Parsing_util in
  parse_using
    (let open Angstrom in
     let blank = take_while1 Char.is_whitespace in
     let+ a = blank *> number
     and+ b = blank *> number
     and+ c = blank *> number in
     a, b, c)
;;

let is_valid (a, b, c) = a + b > c && a + c > b && b + c > a
let parse s = String.split_lines s |> List.map ~f:parse_line
let f1 s = parse s |> List.count ~f:is_valid

let transpose3 (a1, a2, a3) (b1, b2, b3) (c1, c2, c3) =
  [ a1, b1, c1; a2, b2, c2; a3, b3, c3 ]
;;

let rec transpose = function
  | l1 :: l2 :: l3 :: r -> transpose3 l1 l2 l3 @ transpose r
  | [] -> []
  | [ _ ] | [ _; _ ] -> assert false
;;

let f2 s = parse s |> transpose |> List.count ~f:is_valid
let run () = Run.run ~name:"day03" ~f1 ~f2 Day03_input.data
