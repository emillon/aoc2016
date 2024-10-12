type disc =
  { n : int
  ; positions : int
  ; initial : int
  }
[@@deriving sexp]

let parse s =
  String.split_lines s
  |> List.map ~f:(fun s ->
    Stdlib.Scanf.sscanf
      s
      "Disc #%d has %d positions; at time=0, it is at position %d."
      (fun n positions initial -> { n; positions; initial }))
;;

let sample =
  String.concat_lines
    [ "Disc #1 has 5 positions; at time=0, it is at position 4."
    ; "Disc #2 has 2 positions; at time=0, it is at position 1."
    ]
;;

let%expect_test "parse" =
  parse sample |> [%sexp_of: disc list] |> print_s;
  [%expect {| (((n 1) (positions 5) (initial 4)) ((n 2) (positions 2) (initial 1))) |}]
;;

module Crt = struct
  (** x = a (mod n) *)
  type eqn =
    { a : int
    ; n : int
    }

  let combine { a = a1; n = n1 } { a = a2; n = n2 } =
    let g, m1, m2 = Algo.egcd n1 n2 in
    assert (g = 1);
    let x = (a1 * m2 * n2) + (a2 * m1 * n1) in
    let n = n1 * n2 in
    let a = x % n in
    { a; n }
  ;;

  let combine_all = List.reduce_exn ~f:combine
end

let solve l =
  let eqns = List.map l ~f:(fun d -> { Crt.n = d.positions; a = -d.n - d.initial }) in
  let eqn = Crt.combine_all eqns in
  eqn.a
;;

let f1 s = parse s |> solve

let%expect_test "f1" =
  f1 sample |> printf "%d";
  [%expect {| 5 |}]
;;

let alter discs =
  let new_disc = { n = List.length discs + 1; positions = 11; initial = 0 } in
  discs @ [ new_disc ]
;;

let f2 s = parse s |> alter |> solve
let run () = Run.run ~f1 ~f2 Day15_input.data
