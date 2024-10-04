type state =
  | Len of char list
  | Times of
      { len : int
      ; chars : char list
      }
[@@deriving sexp]

let decode_marker s i =
  let get_int l = List.rev l |> String.of_char_list |> Int.of_string in
  let rec go st i =
    let c = String.get s i in
    match st, c with
    | Len l, '0' .. '9' -> go (Len (c :: l)) (i + 1)
    | Len l, 'x' -> go (Times { len = get_int l; chars = [] }) (i + 1)
    | Times { len; chars }, '0' .. '9' -> go (Times { len; chars = c :: chars }) (i + 1)
    | Times { len; chars }, ')' -> len, get_int chars, i + 1
    | _ -> raise_s [%message "decode_marker" (st : state) (c : char)]
  in
  go (Len []) i
;;

type token =
  | Plain of int
  | Marker of
      { times : int
      ; string : string
      }
[@@deriving sexp]

let next_token s i =
  let s_len = String.length s in
  if i < 0 || i >= s_len
  then None
  else
    Some
      (match String.get s i with
       | 'A' .. 'Z' ->
         (match String.index_from s i '(' with
          | Some j -> Plain (j - i), j
          | None -> Plain (s_len - i), -1)
       | '(' ->
         let len, times, after_marker = decode_marker s (i + 1) in
         let string = String.sub ~pos:after_marker ~len s in
         Marker { times; string }, after_marker + len
       | c -> raise_s [%message "next_token" (c : char)])
;;

let fold_tokens s ~init ~f =
  let rec go i acc =
    match next_token s i with
    | None -> acc
    | Some (t, j) -> go j (f acc t)
  in
  go 0 init
;;

let sum_tokens s f_marker =
  fold_tokens s ~init:0 ~f:(fun n t ->
    n
    +
    match t with
    | Plain p -> p
    | Marker { times; string } -> times * f_marker string)
;;

let decompress_len s = sum_tokens s String.length

let%expect_test "decompress_len" =
  let test s = decompress_len s |> printf "%d" in
  test "ADVENT";
  [%expect {| 6 |}];
  test "A(1x5)BC";
  [%expect {| 7 |}];
  test "(3x3)XYZ";
  [%expect {| 9 |}];
  test "A(2x2)BCD(2x2)EFG";
  [%expect {| 11 |}];
  test "(6x1)(1x3)A";
  [%expect {| 6 |}];
  test "X(8x2)(3x3)ABCY";
  [%expect {| 18 |}]
;;

let f1 s = String.strip s |> decompress_len
let rec decompress2 s = sum_tokens s decompress2

let%expect_test "decompress2" =
  let test s = decompress2 s |> printf "%d" in
  test "ADVENT";
  [%expect {| 6 |}];
  test "A(1x5)BC";
  [%expect {| 7 |}];
  test "X(8x2)(3x3)ABCY";
  [%expect {| 20 |}];
  test "(27x12)(20x12)(13x14)(7x10)(1x12)A";
  [%expect {| 241920 |}];
  test "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN";
  [%expect {| 445 |}]
;;

let f2 s = String.strip s |> decompress2
let run () = Run.run ~f1 ~f2 Day09_input.data
