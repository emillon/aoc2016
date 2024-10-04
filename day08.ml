type instr =
  | Rect of
      { width : int
      ; height : int
      }
  | Rotate_column of
      { x : int
      ; offset : int
      }
  | Rotate_row of
      { y : int
      ; offset : int
      }
[@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using
    (choice
       [ (let+ width = string "rect " *> number
          and+ height = string "x" *> number in
          Rect { width; height })
       ; (let+ x = string "rotate column x=" *> number
          and+ offset = string " by " *> number in
          Rotate_column { x; offset })
       ; (let+ y = string "rotate row y=" *> number
          and+ offset = string " by " *> number in
          Rotate_row { y; offset })
       ])
;;

let sample =
  String.concat_lines
    [ "rect 3x2"
    ; "rotate column x=1 by 1"
    ; "rotate row y=0 by 4"
    ; "rotate column x=1 by 1"
    ]
;;

let%expect_test "parse" =
  parse sample |> [%sexp_of: instr list] |> print_s;
  [%expect
    {|
    ((Rect (width 3) (height 2)) (Rotate_column (x 1) (offset 1))
     (Rotate_row (y 0) (offset 4)) (Rotate_column (x 1) (offset 1)))
    |}]
;;

module Screen = struct
  type t =
    { dimx : int
    ; dimy : int
    ; data : bool array array
    }

  let create ~dimx ~dimy =
    let data = Array.make_matrix ~dimx ~dimy false in
    { dimx; dimy; data }
  ;;

  let eval t instr =
    match instr with
    | Rect { width; height } ->
      for i = 0 to width - 1 do
        for j = 0 to height - 1 do
          t.data.(i).(j) <- true
        done
      done
    | Rotate_column { x; offset } ->
      let column = Array.init t.dimy ~f:(fun y -> t.data.(x).(y)) in
      for y = 0 to t.dimy - 1 do
        let y' = (y - offset) % t.dimy in
        t.data.(x).(y) <- column.(y')
      done
    | Rotate_row { y; offset } ->
      let row = Array.init t.dimx ~f:(fun x -> t.data.(x).(y)) in
      for x = 0 to t.dimx - 1 do
        let x' = (x - offset) % t.dimx in
        t.data.(x).(y) <- row.(x')
      done
  ;;

  let repeatn n s =
    let b = Buffer.create 0 in
    for _ = 1 to n do
      Buffer.add_string b s
    done;
    Buffer.contents b
  ;;

  let display t =
    printf "┌%s┐\n" (repeatn t.dimx "─");
    for j = 0 to t.dimy - 1 do
      printf "│";
      for i = 0 to t.dimx - 1 do
        let s = if t.data.(i).(j) then "█" else " " in
        printf "%s" s
      done;
      printf "│\n"
    done;
    printf "└%s┘\n" (repeatn t.dimx "─")
  ;;

  let lit_pixels t = Array.fold t.data ~init:0 ~f:(fun n a -> n + Array.count a ~f:Fn.id)
end

let%expect_test "eval" =
  let queue = parse sample |> Queue.of_list in
  let screen = Screen.create ~dimx:7 ~dimy:3 in
  let go () =
    let instr = Queue.dequeue_exn queue in
    Screen.eval screen instr;
    Screen.display screen
  in
  go ();
  [%expect
    {|
    ┌───────┐
    │███    │
    │███    │
    │       │
    └───────┘
    |}];
  go ();
  [%expect
    {|
    ┌───────┐
    │█ █    │
    │███    │
    │ █     │
    └───────┘
    |}];
  go ();
  [%expect
    {|
    ┌───────┐
    │    █ █│
    │███    │
    │ █     │
    └───────┘
    |}];
  go ();
  [%expect
    {|
    ┌───────┐
    │ █  █ █│
    │█ █    │
    │ █     │
    └───────┘
    |}]
;;

let screen_after_instrs instrs =
  let screen = Screen.create ~dimx:50 ~dimy:6 in
  List.iter ~f:(Screen.eval screen) instrs;
  screen
;;

let f1 s = parse s |> screen_after_instrs |> Screen.lit_pixels

let f2 s =
  parse s |> screen_after_instrs |> Screen.display;
  0
;;

let run () = Run.run ~f1 ~f2 Day08_input.data
