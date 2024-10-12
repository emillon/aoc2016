type dest =
  | Bot of int
  | Output of int
[@@deriving sexp]

type instr =
  | Set of
      { value : int
      ; bot : int
      }
  | Give of
      { bot : int
      ; low : dest
      ; high : dest
      }
[@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  let output =
    choice
      [ (let+ n = string "bot " *> number in
         Bot n)
      ; (let+ n = string "output " *> number in
         Output n)
      ]
  in
  parse_lines_using
  @@ choice
       [ (let+ value = string "value " *> number
          and+ bot = string " goes to bot " *> number in
          Set { value; bot })
       ; (let+ bot = string "bot " *> number
          and+ low = string " gives low to " *> output
          and+ high = string " and high to " *> output in
          Give { bot; low; high })
       ]
;;

let sample =
  String.concat_lines
    [ "value 5 goes to bot 2"
    ; "bot 2 gives low to bot 1 and high to bot 0"
    ; "value 3 goes to bot 1"
    ; "bot 1 gives low to output 1 and high to bot 0"
    ; "bot 0 gives low to output 2 and high to output 0"
    ; "value 2 goes to bot 2"
    ]
;;

let%expect_test "parse" =
  parse sample |> [%sexp_of: instr list] |> print_s;
  [%expect
    {|
    ((Set (value 5) (bot 2)) (Give (bot 2) (low (Bot 1)) (high (Bot 0)))
     (Set (value 3) (bot 1)) (Give (bot 1) (low (Output 1)) (high (Bot 0)))
     (Give (bot 0) (low (Output 2)) (high (Output 0))) (Set (value 2) (bot 2)))
    |}]
;;

type t =
  { values : int Map.M(Int).t
  ; mappings : (dest * dest) Map.M(Int).t
  ; outputs : int list Map.M(Int).t
  }
[@@deriving sexp]

let empty =
  let empty_map = Map.empty (module Int) in
  { values = empty_map; mappings = empty_map; outputs = empty_map }
;;

let sorted a b = if a < b then a, b else b, a

let rec give t dest value ~on_compare =
  match dest with
  | Bot bot ->
    (match Map.find t.values bot with
     | None -> { t with values = Map.add_exn t.values ~key:bot ~data:value }
     | Some v0 ->
       let low, high =
         Map.find t.mappings bot
         |> Option.value_or_thunk ~default:(fun () ->
           raise_s [%message "no mapping" (t : t) (bot : int)])
       in
       let v_low, v_high = sorted v0 value in
       on_compare bot (v_low, v_high);
       let t = { t with values = Map.remove t.values bot } in
       let t = give t low v_low ~on_compare in
       let t = give t high v_high ~on_compare in
       t)
  | Output n -> { t with outputs = Map.add_multi t.outputs ~key:n ~data:value }
;;

let sort_instrs l =
  let gives, sets =
    List.partition_tf l ~f:(function
      | Give _ -> true
      | Set _ -> false)
  in
  gives @ sets
;;

let interpret ~on_compare instrs =
  sort_instrs instrs
  |> List.fold ~init:empty ~f:(fun t instr ->
    match instr with
    | Set { value; bot } -> give t (Bot bot) value ~on_compare
    | Give { bot; low; high } ->
      { t with mappings = Map.add_exn t.mappings ~key:bot ~data:(low, high) })
;;

let%expect_test "interpret" =
  parse sample
  |> interpret ~on_compare:(fun bot values ->
    print_s [%message "comparing" (bot : int) (values : int * int)])
  |> [%sexp_of: t]
  |> print_s;
  [%expect
    {|
    (comparing (bot 2) (values (2 5)))
    (comparing (bot 1) (values (2 3)))
    (comparing (bot 0) (values (3 5)))
    ((values ())
     (mappings
      ((0 ((Output 2) (Output 0))) (1 ((Output 1) (Bot 0)))
       (2 ((Bot 1) (Bot 0)))))
     (outputs ((0 (5)) (1 (2)) (2 (3)))))
    |}]
;;

let sim_all s =
  let r = ref None in
  let t =
    parse s
    |> interpret ~on_compare:(fun bot (a, b) ->
      match a, b with
      | 17, 61 -> r := Some bot
      | _ -> ())
  in
  Option.value_exn !r, t
;;

let f1 s = fst (sim_all s)
let sole_output t n = Algo.sole (Map.find_exn t.outputs n)

let f2 s =
  let t = snd (sim_all s) in
  sole_output t 0 * sole_output t 1 * sole_output t 2
;;

let run () = Run.run ~f1 ~f2 Day10_input.data
