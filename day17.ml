let is_valid (x, y) = 1 <= x && x <= 4 && 1 <= y && y <= 4
let is_winning pos = Vec.equal pos (4, 1)
let md5 s = Stdlib.Digest.string s |> Stdlib.Digest.to_hex

let char_open = function
  | 'b' .. 'f' -> true
  | _ -> false
;;

let next pos code =
  let digest = md5 code in
  List.filter_mapi
    [ (0, 1), 'U'; (0, -1), 'D'; (-1, 0), 'L'; (1, 0), 'R' ]
    ~f:(fun i (v, c) ->
      if char_open digest.[i]
      then Some (Vec.add pos v, Printf.sprintf "%s%c" code c)
      else None)
;;

let solve prefix ~on_win =
  let q = Queue.create () in
  Queue.enqueue q ((1, 4), prefix);
  while not (Queue.is_empty q) do
    let pos, code = Queue.dequeue_exn q in
    if is_valid pos
    then if is_winning pos then on_win code else next pos code |> Queue.enqueue_all q
  done
;;

let solve_shortest prefix =
  let exception Found of string in
  match solve prefix ~on_win:(fun code -> raise (Found code)) with
  | () -> None
  | exception Found s -> Some (String.chop_prefix_exn ~prefix s)
;;

let sample = String.concat_lines [ "ihgpwlah" ]
let parse s = String.strip s

let%expect_test "solve_shortest" =
  let test s = solve_shortest s |> [%sexp_of: string option] |> print_s in
  test (parse sample);
  [%expect {| (DDRRRD) |}];
  test "hijkl";
  [%expect {| () |}];
  test "kglvqrro";
  [%expect {| (DDUDRLRRUDRD) |}];
  test "ulqzkmiv";
  [%expect {| (DRURDRUDDLLDLUURRDULRLDUUDDDRR) |}]
;;

let f1 s = parse s |> solve_shortest |> Option.value_exn

let solve_longest prefix =
  let longest = ref "" in
  solve prefix ~on_win:(fun code ->
    if String.length code > String.length !longest then longest := code);
  let longest_path = String.chop_prefix_exn ~prefix !longest in
  String.length longest_path
;;

let%expect_test "solve_longest" =
  let test s = solve_longest s |> printf "%d" in
  test "ihgpwlah";
  [%expect {| 370 |}];
  test "kglvqrro";
  [%expect {| 492 |}];
  test "ulqzkmiv";
  [%expect {| 830 |}]
;;

let f2 s = parse s |> solve_longest |> Int.to_string
let run () = Run.run_string ~f1 ~f2 Day17_input.data
