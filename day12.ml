module Register = struct
  module T = struct
    type t =
      | A
      | B
      | C
      | D
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let enum = [ "a", A; "b", B; "c", C; "d", D ]
end

type src =
  | Int of int
  | Reg of Register.t

type instr =
  | Cpy of
      { src : src
      ; dst : Register.t
      }
  | Jnz of
      { src : src
      ; offset : int
      }
  | Add of
      { register : Register.t
      ; n : int
      }

let parse =
  let open Parsing_util in
  let open Angstrom in
  let register = enum Register.enum in
  let src =
    choice
      [ (let+ n = number in
         Int n)
      ; (let+ r = register in
         Reg r)
      ]
  in
  parse_lines_using
    (choice
       [ (let+ src = string "cpy " *> src
          and+ dst = string " " *> register in
          Cpy { src; dst })
       ; (let+ src = string "jnz " *> src
          and+ offset = string " " *> signed_number in
          Jnz { src; offset })
       ; (let+ register = string "inc " *> register in
          Add { register; n = 1 })
       ; (let+ register = string "dec " *> register in
          Add { register; n = -1 })
       ])
;;

type t =
  { instrs : instr Map.M(Int).t
  ; ip : int
  ; regs : int Map.M(Register).t
  }

let state_from_instrs instrs =
  { instrs =
      List.mapi instrs ~f:(fun i instr -> i, instr) |> Map.of_alist_exn (module Int)
  ; ip = 0
  ; regs = Map.empty (module Register)
  }
;;

let ( .%{} ) t = function
  | Int n -> n
  | Reg r -> Map.find t.regs r |> Option.value ~default:0
;;

let ( .%{}<- ) t r v = { t with regs = Map.set t.regs ~key:r ~data:v }
let jump off t = { t with ip = t.ip + off }

let instr_offset t = function
  | Cpy _ -> 1
  | Add _ -> 1
  | Jnz { src; offset } ->
    (match t.%{src} with
     | 0 -> 1
     | _ -> offset)
;;

let exec_one t = function
  | Cpy { src; dst } -> t.%{dst} <- t.%{src}
  | Add { register; n } -> t.%{register} <- t.%{Reg register} + n
  | Jnz _ -> t
;;

let rec exec_all t =
  match Map.find t.instrs t.ip with
  | None -> t
  | Some instr -> exec_one t instr |> jump (instr_offset t instr) |> exec_all
;;

let f1 s =
  let state = parse s |> state_from_instrs |> exec_all in
  Map.find_exn state.regs A
;;

let set_c t = { t with regs = Map.set t.regs ~key:C ~data:1 }

let sample =
  String.concat_lines [ "cpy 41 a"; "inc a"; "inc a"; "dec a"; "jnz a 2"; "dec a" ]
;;

let%expect_test "f1" =
  f1 sample |> printf "%d";
  [%expect {| 42 |}]
;;

let f2 s =
  let state = parse s |> state_from_instrs |> set_c |> exec_all in
  Map.find_exn state.regs A
;;

let run () = Run.run ~f1 ~f2 Day12_input.data
