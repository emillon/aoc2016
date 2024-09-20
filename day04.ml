type room =
  { letters : char list
  ; id : int
  ; checksum : string
  ; original : string
  }

let parse_line =
  Parsing_util.parse_using
    (let open Angstrom in
     let+ original, letters =
       scan [] (fun l c ->
         match c with
         | '-' -> Some l
         | 'a' .. 'z' -> Some (c :: l)
         | _ -> None)
     and+ id = Parsing_util.number
     and+ checksum = char '[' *> Parsing_util.word <* char ']' in
     { letters; id; checksum; original })
;;

let swap (x, y) = y, x
let take n l = List.take l n

let count m l =
  List.fold l ~init:(Map.empty m) ~f:(fun count c ->
    Map.update count c ~f:(fun n -> 1 + Option.value n ~default:0))
;;

let checksum l =
  l
  |> count (module Char)
  |> Map.to_alist
  |> List.map ~f:swap
  |> List.sort ~compare:[%compare: int * char Comparable.reversed]
  |> List.rev
  |> take 5
  |> List.map ~f:snd
  |> String.of_char_list
;;

let%expect_test "checksum" =
  let test s =
    let r = checksum (String.to_list s) in
    print_s [%message r]
  in
  test "aaaaabbbzyx";
  [%expect {| abxyz |}]
;;

let is_valid r = String.equal (checksum r.letters) r.checksum
let valid_room_id r = Option.some_if (is_valid r) r.id
let parse s = String.split_lines s |> List.map ~f:parse_line
let sum = List.fold ~init:0 ~f:( + )
let f1 s = parse s |> List.filter_map ~f:valid_room_id |> sum

let shift =
  String.map ~f:(function
    | 'a' .. 'y' as c -> Char.to_int c |> Int.succ |> Char.of_int_exn
    | 'z' -> 'a'
    | '-' -> '-'
    | c -> raise_s [%message (c : char)])
;;

let decrypt s = Fn.apply_n_times ~n:s.id shift s.original
let is_ok s = String.is_prefix s ~prefix:"north"

let f2 s =
  parse s
  |> List.find_map_exn ~f:(fun r ->
    let decrypted = decrypt r in
    Option.some_if (is_ok decrypted) r.id)
;;

let run () = Run.run ~f1 ~f2 Day04_input.data
