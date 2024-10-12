let find_3 s =
  List.range 0 (String.length s - 2)
  |> List.find_map ~f:(fun i ->
    let c0 = s.[i] in
    let c1 = s.[i + 1] in
    let c2 = s.[i + 2] in
    Option.some_if (Char.equal c0 c1 && Char.equal c1 c2) c0)
;;

let%expect_test "find_3" =
  let test s = find_3 s |> [%sexp_of: char option] |> print_s in
  test "0034e0923cc38887a57bd7b1d4f953df";
  [%expect {| (8) |}];
  test "347dac6ee8eeea4652c7476d0f97bee5";
  [%expect {| (e) |}];
  test "ae2e85dd75d63e916a525df95e999ea0";
  [%expect {| (9) |}];
  test "aaabcccd";
  [%expect {| (a) |}]
;;

let has_5 s c =
  List.range 0 (String.length s - 4)
  |> List.exists ~f:(fun i ->
    let open Char in
    c = s.[i] && c = s.[i + 1] && c = s.[i + 2] && c = s.[i + 3] && c = s.[i + 4])
;;

let md5 s = Digestif.MD5.digest_string s |> Digestif.MD5.to_hex

let solve ~stretched salt =
  let hash_expensive n =
    let runs = if stretched then 2017 else 1 in
    Fn.apply_n_times ~n:runs md5 (Printf.sprintf "%s%d" salt n)
  in
  let cache = Hashtbl.create (module Int) in
  let hash n =
    match Hashtbl.find cache n with
    | Some r -> r
    | None ->
      let r = hash_expensive n in
      Hashtbl.add_exn cache ~key:n ~data:r;
      r
  in
  let find_3_n n = find_3 (hash n) in
  let has_5_n n c = has_5 (hash n) c in
  let is_ok n =
    match find_3_n n with
    | None -> false
    | Some c ->
      List.exists (List.range ~start:`inclusive 1 ~stop:`inclusive 1000) ~f:(fun i ->
        has_5_n (n + i) c)
  in
  let rec next_key n = if is_ok n then n else next_key (n + 1) in
  let rec nth_key ~n ~after =
    if n = 0
    then after - 1
    else (
      let key = next_key after in
      nth_key ~n:(n - 1) ~after:(key + 1))
  in
  nth_key ~n:64 ~after:0
;;

let parse s = String.strip s
let f1 s = parse s |> solve ~stretched:false
let f2 s = parse s |> solve ~stretched:true
let run () = Run.run ~f1 ~f2 Day14_input.data
