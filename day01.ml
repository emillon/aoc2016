let repeat n l = List.init n ~f:(fun _ -> l) |> List.concat

module Instr = struct
  type t =
    | Rotate of Vec.t
    | Forward
  [@@deriving sexp]

  let dir_of_char = function
    | 'R' -> 0, -1
    | 'L' -> 0, 1
    | _ -> assert false
  ;;

  let parse s =
    Stdlib.Scanf.sscanf s "%c%d" (fun c n ->
      [ Rotate (dir_of_char c) ] @ repeat n [ Forward ] @ [])
  ;;

  let check = function
    | Rotate _ -> false
    | Forward -> true
  ;;
end

module State = struct
  type t =
    { pos : Vec.t
    ; dir : Vec.t
    }
  [@@deriving sexp]

  let initial = { pos = Vec.zero; dir = Vec.one }

  let next s (i : Instr.t) =
    match i with
    | Rotate r -> { s with dir = Vec.cmul s.dir r }
    | Forward -> { s with pos = Vec.add s.pos s.dir }
  ;;

  let pos t = t.pos
end

let parse s =
  String.split ~on:',' s |> List.concat_map ~f:(fun s -> Instr.parse (String.strip s))
;;

let f1 s =
  parse s |> List.fold_left ~init:State.initial ~f:State.next |> State.pos |> Vec.l1_norm
;;

module State2 = struct
  type t =
    { state : State.t
    ; visited : Set.M(Vec).t
    }
  [@@deriving sexp]

  let initial = { state = State.initial; visited = Set.empty (module Vec) }

  let rec go s2 = function
    | instr :: r ->
      let nextstate = State.next s2.state instr in
      let nextpos = State.pos nextstate in
      if Instr.check instr
      then
        if Set.mem s2.visited nextpos
        then Vec.l1_norm nextpos
        else go { state = nextstate; visited = Set.add s2.visited nextpos } r
      else go { state = nextstate; visited = s2.visited } r
    | [] -> raise_s [%message (s2 : t)]
  ;;
end

let f2 s = parse s |> State2.go State2.initial

let%expect_test _ =
  f2 "R8, R4, R4, R8" |> [%sexp_of: int] |> print_s;
  [%expect {| 4 |}]
;;

let run () = Run.run ~f1 ~f2 Day01_input.data
