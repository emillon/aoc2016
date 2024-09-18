let number =
  let open Angstrom in
  let+ s = take_while1 Char.is_digit in
  Int.of_string s
;;

let word =
  let open Angstrom in
  take_while1 Char.is_alpha
;;

let parse_using p s =
  match Angstrom.parse_string ~consume:All p s with
  | Ok x -> x
  | Error e -> Printf.ksprintf failwith "While parsing %S: %s" s e
;;

let enum l =
  let open Angstrom in
  List.map l ~f:(fun (s, r) -> string s *> return r) |> choice
;;
