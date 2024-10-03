type t =
  { normal : string list
  ; hyper : string list
  }

let add_normal t s = { t with normal = s :: t.normal }
let add_hyper t s = { t with hyper = s :: t.hyper }
let of_rev_char_list cs = List.rev cs |> String.of_char_list

let parse_one s =
  let left, t =
    String.fold
      s
      ~init:([], { normal = []; hyper = [] })
      ~f:(fun (cur, acc) c ->
        match c with
        | 'a' .. 'z' -> c :: cur, acc
        | '[' -> [], add_normal acc (of_rev_char_list cur)
        | ']' -> [], add_hyper acc (of_rev_char_list cur)
        | _ -> raise_s [%message "parse_one" (cur : char list) (c : char)])
  in
  add_normal t (of_rev_char_list left)
;;

let parse s = String.split_lines s |> List.map ~f:parse_one

let has_abba s =
  let s_len = String.length s in
  let found = ref false in
  if s_len >= 4
  then
    for i = 0 to s_len - 4 do
      let a = String.get s i in
      let b = String.get s (i + 1) in
      let b' = String.get s (i + 2) in
      let a' = String.get s (i + 3) in
      if Char.(a = a' && b = b' && a <> b) then found := true
    done;
  !found
;;

let is_tls { normal; hyper } =
  List.exists normal ~f:has_abba && List.for_all hyper ~f:(fun s -> not (has_abba s))
;;

let f1 s = parse s |> List.count ~f:is_tls

let find_aba_in s =
  let s_len = String.length s in
  let r = ref [] in
  if s_len >= 3
  then
    for i = 0 to s_len - 3 do
      let a = String.get s i in
      let b = String.get s (i + 1) in
      let a' = String.get s (i + 2) in
      if Char.(a <> b && a = a') then r := (a, b) :: !r
    done;
  !r
;;

let find_aba t = List.concat_map t.normal ~f:find_aba_in

let has_bab t (a, b) =
  let needle = Printf.ksprintf String.Search_pattern.create "%c%c%c" b a b in
  List.exists t.hyper ~f:(String.Search_pattern.matches needle)
;;

let is_ssl t =
  match find_aba t with
  | [] -> false
  | abas -> List.exists abas ~f:(fun aba -> has_bab t aba)
;;

let%expect_test "is_ssl" =
  let test s = parse_one s |> is_ssl |> printf "%b" in
  test "aba[bab]xyz";
  [%expect {| true |}];
  test "xyx[xyx]xyx";
  [%expect {| false |}];
  test "aaa[kek]eke";
  [%expect {| true |}];
  test "zazbz[bzb]cdb";
  [%expect {| true |}]
;;

let f2 s = parse s |> List.count ~f:is_ssl
let run () = Run.run ~f1 ~f2 Day07_input.data
