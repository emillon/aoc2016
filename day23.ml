let f1 s = Monorail.parse s |> Monorail.set ~reg:A ~data:7 |> Monorail.exec

let sample =
  String.concat_lines
    [ "cpy 2 a"; "tgl a"; "tgl a"; "tgl a"; "cpy 1 a"; "dec a"; "dec a" ]
;;

let%expect_test "f1" =
  f1 sample |> printf "%d";
  [%expect {| 3 |}]
;;

let f2 s =
  Monorail.parse s |> Monorail.optimize |> Monorail.set ~reg:A ~data:12 |> Monorail.exec
;;

let run () = Run.run ~f1 ~f2 Day23_input.data
