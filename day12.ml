let f1 s = Monorail.parse s |> Monorail.exec

let sample =
  String.concat_lines [ "cpy 41 a"; "inc a"; "inc a"; "dec a"; "jnz a 2"; "dec a" ]
;;

let%expect_test "f1" =
  f1 sample |> printf "%d";
  [%expect {| 42 |}]
;;

let f2 s = Monorail.parse s |> Monorail.set ~reg:C ~data:1 |> Monorail.exec
let run () = Run.run ~f1 ~f2 Day12_input.data
