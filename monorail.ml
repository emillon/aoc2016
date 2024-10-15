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
[@@deriving equal, sexp]

type instr =
  | Cpy of
      { src : src
      ; dst : Register.t
      }
  | Jnz of
      { src : src
      ; offset : src
      }
  | Inc of Register.t
  | Dec of Register.t
  | Tgl of Register.t
  | Mul of
      { src1 : Register.t
      ; src2 : Register.t
      ; dst : Register.t
      }
  | Nop
[@@deriving equal, sexp]

type t =
  { instrs : instr Map.M(Int).t
  ; ip : int
  ; regs : int Map.M(Register).t
  }

let get t ~reg = Map.find_exn t.regs reg
let set t ~reg ~data = { t with regs = Map.set t.regs ~key:reg ~data }

let state_from_instrs instrs =
  { instrs =
      List.mapi instrs ~f:(fun i instr -> i, instr) |> Map.of_alist_exn (module Int)
  ; ip = 0
  ; regs = Map.empty (module Register)
  }
;;

let parse s =
  let open Parsing_util in
  let open Angstrom in
  let register = enum Register.enum in
  let src =
    choice
      [ (let+ n = signed_number in
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
          and+ offset = string " " *> src in
          Jnz { src; offset })
       ; (let+ r = string "inc " *> register in
          Inc r)
       ; (let+ r = string "dec " *> register in
          Dec r)
       ; (let+ r = string "tgl " *> register in
          Tgl r)
       ])
    s
  |> state_from_instrs
;;

let ( .%{} ) t = function
  | Int n -> n
  | Reg r -> Map.find t.regs r |> Option.value ~default:0
;;

let ( .%{}<- ) t r v = { t with regs = Map.set t.regs ~key:r ~data:v }
let jump off t = { t with ip = t.ip + off }

let instr_offset t = function
  | Cpy _ -> 1
  | Inc _ -> 1
  | Dec _ -> 1
  | Jnz { src; offset } ->
    (match t.%{src} with
     | 0 -> 1
     | _ -> t.%{offset})
  | Tgl _ -> 1
  | Nop -> 1
  | Mul _ -> 1
;;

let toggle_instr = function
  | Tgl r -> Inc r
  | Cpy { src; dst } -> Jnz { src; offset = Reg dst }
  | Inc r -> Dec r
  | Jnz { src; offset = Reg r } -> Cpy { src; dst = r }
  | instr -> raise_s [%message "toggle_instr" (instr : instr)]
;;

let add t r n = t.%{r} <- t.%{Reg r} + n

let exec_one t = function
  | Cpy { src; dst } -> t.%{dst} <- t.%{src}
  | Inc r -> add t r 1
  | Dec r -> add t r (-1)
  | Mul { src1; src2; dst } -> add t dst (t.%{Reg src1} * t.%{Reg src2})
  | Jnz _ -> t
  | Tgl r ->
    { t with
      instrs = Map.change t.instrs (t.ip + t.%{Reg r}) ~f:(Option.map ~f:toggle_instr)
    }
  | Nop -> t
;;

let rec exec_all t =
  match Map.find t.instrs t.ip with
  | None -> t
  | Some instr -> exec_one t instr |> jump (instr_offset t instr) |> exec_all
;;

let exec t = exec_all t |> get ~reg:A

let set_instr t ~ip ~instr ~expected =
  let existing = Map.find_exn t.instrs ip in
  assert (equal_instr existing expected);
  let instrs = Map.set t.instrs ~key:ip ~data:instr in
  { t with instrs }
;;

let optimize t =
  t
  |> set_instr ~ip:5 ~expected:(Inc A) ~instr:(Mul { src1 = C; src2 = D; dst = A })
  |> set_instr ~ip:6 ~expected:(Dec C) ~instr:Nop
  |> set_instr ~ip:7 ~expected:(Jnz { src = Reg C; offset = Int (-2) }) ~instr:Nop
  |> set_instr ~ip:8 ~expected:(Dec D) ~instr:Nop
  |> set_instr ~ip:9 ~expected:(Jnz { src = Reg D; offset = Int (-5) }) ~instr:Nop
;;
