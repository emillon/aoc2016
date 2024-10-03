let sample =
  String.concat_lines
    [ "eedadn"
    ; "drvtee"
    ; "eandsr"
    ; "raavrd"
    ; "atevrs"
    ; "tsrnev"
    ; "sdttsa"
    ; "rasrtv"
    ; "nssdts"
    ; "ntnada"
    ; "svetve"
    ; "tesnvt"
    ; "vntsnd"
    ; "vrdear"
    ; "dvrsen"
    ; "enarar"
    ]
;;

let parse s =
  String.split_lines s
  |> List.map ~f:String.to_list
  |> List.transpose_exn
  |> List.map ~f:(fun column ->
    List.fold
      column
      ~init:(Map.empty (module Char))
      ~f:(fun m c ->
        Map.update m c ~f:(fun no ->
          let n = Option.value no ~default:0 in
          n + 1)))
;;

let%expect_test "parse" =
  parse sample |> [%sexp_of: int Map.M(Char).t list] |> print_s;
  [%expect
    {|
    (((a 1) (d 2) (e 3) (n 2) (r 2) (s 2) (t 2) (v 2))
     ((a 3) (d 1) (e 2) (n 2) (r 2) (s 2) (t 2) (v 2))
     ((a 2) (d 2) (e 2) (n 2) (r 2) (s 3) (t 2) (v 1))
     ((a 2) (d 2) (e 1) (n 2) (r 2) (s 2) (t 3) (v 2))
     ((a 2) (d 2) (e 3) (n 1) (r 2) (s 2) (t 2) (v 2))
     ((a 2) (d 2) (e 2) (n 2) (r 3) (s 2) (t 1) (v 2)))
    |}]
;;

let f_gen s how =
  let largest = Comparable.lift ~f:snd Int.compare in
  let compare =
    match how with
    | `Max -> largest
    | `Min -> Comparable.reverse largest
  in
  parse s
  |> List.map ~f:(fun m ->
    Map.to_alist m |> List.max_elt ~compare |> Option.value_exn |> fst)
  |> String.of_char_list
;;

let f1 s = f_gen s `Max

let%expect_test "f1" =
  f1 sample |> print_string;
  [%expect {| easter |}]
;;

let f2 s = f_gen s `Min

let%expect_test "f2" =
  f2 sample |> print_string;
  [%expect {| advent |}]
;;

let run () = Run.run_string ~f1 ~f2 Day06_input.data
