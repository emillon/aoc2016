let other = function
  | '0' -> '1'
  | _ -> '0'
;;

let compute_checksum a =
  let step n =
    let checksum_len = n / 2 in
    for i = 0 to checksum_len - 1 do
      let c1 = Array.unsafe_get a (2 * i) in
      let c2 = Array.unsafe_get a ((2 * i) + 1) in
      let c = if Char.equal c1 c2 then '1' else '0' in
      Array.unsafe_set a i c
    done;
    checksum_len
  in
  let rec go n =
    let c_len = step n in
    if c_len % 2 = 0 then go c_len else Array.subo ~len:c_len a |> Array.to_list
  in
  go (Array.length a)
;;

let fill_and_checksum initial n =
  let a = Array.create ~len:n 'x' in
  let used = ref (List.length initial) in
  List.iteri initial ~f:(fun i c -> a.(i) <- c);
  let step () =
    let m = !used in
    a.(m) <- '0';
    for i = m - 1 downto 0 do
      let j = (2 * m) - i in
      a.(j) <- other a.(i)
    done;
    used := (2 * !used) + 1
  in
  try
    while true do
      step ()
    done;
    assert false
  with
  | Invalid_argument _ -> compute_checksum a
;;

let%expect_test "fill_and_checksum" =
  fill_and_checksum (String.to_list "10000") 20 |> [%sexp_of: char list] |> print_s;
  [%expect {| (0 1 1 0 0) |}]
;;

let parse s = s |> String.strip |> String.to_list
let to_string = String.of_char_list

let f1 s =
  let l = parse s in
  fill_and_checksum l 272 |> to_string
;;

let f2 s =
  let l = parse s in
  fill_and_checksum l 35651584 |> to_string
;;

let run () = Run.run_string ~f1 ~f2 Day16_input.data
