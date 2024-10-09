module D = Diet.Int
module I = D.Interval

let all_ips = D.add (I.make 0 0xff_ff_ff_ff) D.empty

let parse s =
  let open Parsing_util in
  let open Angstrom in
  let ranges =
    parse_lines_using
      (let+ x = number <* char '-'
       and+ y = number in
       I.make x y)
      s
  in
  let deny_list = List.fold ranges ~init:D.empty ~f:(Fn.flip D.add) in
  D.diff all_ips deny_list
;;

let f1 s = parse s |> D.min_elt |> I.x
let sample = String.concat_lines [ "5-8"; "0-2"; "4-7" ]

let%expect_test "f1" =
  f1 sample |> printf "%d";
  [%expect {| 3 |}]
;;

let f2 s = parse s |> D.cardinal
let run () = Run.run ~f1 ~f2 Day20_input.data
