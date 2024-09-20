type hash_info =
  { first_char : char
  ; second_char : char
  }
[@@deriving sexp]

let bytes_zeroes = function
  | '\x00' -> 2
  | '\x01' .. '\x0f' -> 1
  | '\x10' .. '\xff' -> 0
;;

let count_leading_hex_zeroes s =
  let s_len = String.length s in
  let rec go i r =
    if i >= s_len
    then r
    else (
      let z = bytes_zeroes (String.unsafe_get s i) in
      let r' = r + z in
      if z = 2 then go (i + 1) r' else r')
  in
  go 0 0
;;

let hash_info h =
  let difficulty = 5 in
  let zeroes = count_leading_hex_zeroes (Digestif.MD5.to_raw_string h) in
  if zeroes >= difficulty
  then (
    let as_hex = Digestif.MD5.to_hex h in
    let first_char = String.get as_hex difficulty in
    let second_char = String.get as_hex (difficulty + 1) in
    Some { first_char; second_char })
  else None
;;

let hash d_s = Digestif.MD5.get d_s |> hash_info
let hash_prefix s = Digestif.MD5.feed_string Digestif.MD5.empty s

let%expect_test _ =
  let test s = hash_prefix s |> hash |> [%sexp_of: hash_info option] |> print_s in
  test "abc3231929";
  [%expect {| (((first_char 1) (second_char 5))) |}];
  test "abc5017308";
  [%expect {| (((first_char 8) (second_char f))) |}];
  test "abc5278568";
  [%expect {| (((first_char f) (second_char 9))) |}]
;;

let rec password_after d_s i =
  match hash (Digestif.MD5.feed_string d_s (Int.to_string i)) with
  | Some h -> h, i + 1
  | None -> password_after d_s (i + 1)
;;

let password s =
  let d_s = hash_prefix s in
  let i = ref 0 in
  let b = Buffer.create 0 in
  for _ = 1 to 8 do
    let { first_char = c; _ }, n = password_after d_s !i in
    i := n;
    Buffer.add_char b c
  done;
  Buffer.contents b
;;

let%expect_test _ =
  password "abc" |> print_endline;
  [%expect {| 18f47a30 |}]
;;

let f1 s = String.strip s |> password

let pos_of_char = function
  | '0' .. '7' as c -> Some (Char.to_int c - Char.to_int '0')
  | _ -> None
;;

let password2 s =
  let d_s = hash_prefix s in
  let i = ref 0 in
  let unset = 'x' in
  let b = Bytes.make 8 unset in
  let found = ref 0 in
  while not (!found = 8) do
    let { first_char = pos_char; second_char = value }, n = password_after d_s !i in
    i := n;
    match pos_of_char pos_char with
    | Some pos when Char.equal (Bytes.get b pos) unset ->
      Bytes.set b pos value;
      Int.incr found
    | _ -> ()
  done;
  Bytes.to_string b
;;

let f2 s = String.strip s |> password2
let run () = Run.run_string ~f1 ~f2 Day05_input.data
