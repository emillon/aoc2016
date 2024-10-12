type direction =
  | Left
  | Right
[@@deriving sexp]

type instr =
  | Swap_position of int * int
  | Swap_letter of char * char
  | Reverse of
      { start : int
      ; stop : int
      }
  | Rotate of direction * int
  | Move_position of
      { src : int
      ; dst : int
      }
  | Rotate_based_on of char
  | Invert_rotate_based_on of char
[@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  let direction = enum [ "left", Left; "right", Right ] in
  parse_lines_using
    (choice
       [ (let+ a = string "swap position " *> number
          and+ b = string " with position " *> number in
          Swap_position (a, b))
       ; (let+ a = string "swap letter " *> any_char
          and+ b = string " with letter " *> any_char in
          Swap_letter (a, b))
       ; (let+ start = string "reverse positions " *> number
          and+ stop = string " through " *> number in
          Reverse { start; stop })
       ; (let+ direction = string "rotate " *> direction <* string " "
          and+ n = number <* string " step" <* option ' ' (char 's') in
          Rotate (direction, n))
       ; (let+ src = string "move position " *> number
          and+ dst = string " to position " *> number in
          Move_position { src; dst })
       ; (let+ c = string "rotate based on position of letter " *> any_char in
          Rotate_based_on c)
       ])
;;

let sample =
  String.concat_lines
    [ "swap position 4 with position 0"
    ; "swap letter d with letter b"
    ; "reverse positions 0 through 4"
    ; "rotate left 1 step"
    ; "move position 1 to position 4"
    ; "move position 3 to position 0"
    ; "rotate based on position of letter b"
    ; "rotate based on position of letter d"
    ]
;;

let rotate s i =
  let s_n = String.length s in
  let head_len = i % s_n in
  let head = String.subo s ~len:head_len in
  let tail = String.subo s ~pos:head_len in
  String.append tail head
;;

let rotate_based_on s c =
  let i = String.index_exn s c in
  let n = i + 1 + if i >= 4 then 1 else 0 in
  rotate s (-n)
;;

let exec s = function
  | Swap_position (a, b) ->
    String.mapi s ~f:(fun i c ->
      match () with
      | _ when i = a -> s.[b]
      | _ when i = b -> s.[a]
      | _ -> c)
  | Swap_letter (a, b) ->
    String.map s ~f:(function
      | c when Char.equal c a -> b
      | c when Char.equal c b -> a
      | c -> c)
  | Reverse { start; stop } ->
    String.mapi s ~f:(fun i c ->
      if start <= i && i <= stop then s.[start + stop - i] else c)
  | Rotate (Left, n) -> rotate s n
  | Rotate (Right, n) -> rotate s (-n)
  | Move_position { src; dst } ->
    assert (src <> dst);
    if src < dst
    then
      String.mapi s ~f:(fun i c ->
        match () with
        | _ when i < src -> c
        | _ when i < dst -> s.[i + 1]
        | _ when i = dst -> s.[src]
        | _ -> c)
    else
      String.mapi s ~f:(fun i c ->
        match () with
        | _ when i < dst -> c
        | _ when i = dst -> s.[src]
        | _ when i <= src -> s.[i - 1]
        | _ -> c)
  | Rotate_based_on c -> rotate_based_on s c
  | Invert_rotate_based_on c ->
    let possible_rotations = List.range 0 (String.length s) in
    let candidates =
      List.filter_map possible_rotations ~f:(fun i ->
        let candidate = rotate s i in
        let s' = rotate_based_on candidate c in
        Option.some_if (String.equal s' s) candidate)
    in
    Algo.sole candidates
;;

let%expect_test "exec" =
  let q = Queue.of_list (parse sample) in
  let s = ref "abcde" in
  let step () =
    let instr = Queue.dequeue_exn q in
    let result = exec !s instr in
    s := result;
    print_s [%message (instr : instr) result]
  in
  step ();
  [%expect {| ((instr (Swap_position 4 0)) ebcda) |}];
  step ();
  [%expect {| ((instr (Swap_letter d b)) edcba) |}];
  step ();
  [%expect {| ((instr (Reverse (start 0) (stop 4))) abcde) |}];
  step ();
  [%expect {| ((instr (Rotate Left 1)) bcdea) |}];
  step ();
  [%expect {| ((instr (Move_position (src 1) (dst 4))) bdeac) |}];
  step ();
  [%expect {| ((instr (Move_position (src 3) (dst 0))) abdec) |}];
  step ();
  [%expect {| ((instr (Rotate_based_on b)) ecabd) |}];
  step ();
  [%expect {| ((instr (Rotate_based_on d)) decab) |}]
;;

let reverse_instr = function
  | Move_position { src; dst } -> Move_position { dst = src; src = dst }
  | (Reverse _ | Swap_position _ | Swap_letter _) as i -> i
  | Rotate_based_on c -> Invert_rotate_based_on c
  | Rotate (Left, n) -> Rotate (Right, n)
  | Rotate (Right, n) -> Rotate (Left, n)
  | Invert_rotate_based_on _ -> assert false
;;

let reverse_instrs = List.rev_map ~f:reverse_instr
let f1 s = parse s |> List.fold ~init:"abcdefgh" ~f:exec
let f2 s = parse s |> reverse_instrs |> List.fold ~init:"fbgdceah" ~f:exec
let run () = Run.run_string ~f1 ~f2 Day21_input.data
