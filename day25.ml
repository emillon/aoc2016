let exec_and_listen t =
  let r = ref None in
  let t = Monorail.set_out_handler t ~f:(fun n -> r := Some n) in
  let t' = Monorail.step t in
  t', !r
;;

let rec get_next t =
  match exec_and_listen t with
  | Some t', None -> get_next t'
  | Some t', Some o -> t', o
  | None, _ -> assert false
;;

let is_ok t =
  let rec go t ~expect i =
    i >= 10
    ||
    let t', out = get_next t in
    expect = out && go t' ~expect:(1 - expect) (i + 1)
  in
  go t ~expect:0 0
;;

let solve t =
  let rec go n = if is_ok (Monorail.set t ~reg:A ~data:n) then n else go (n + 1) in
  go 0
;;

let f1 s = Monorail.parse s |> solve
let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day25_input.data
