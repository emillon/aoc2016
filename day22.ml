type node =
  { used : int
  ; avail : int
  }
[@@deriving sexp]

type t = node Map.M(Vec).t [@@deriving sexp]

let is_viable_pair t (ka, kb) =
  let a = Map.find_exn t ka in
  let b = Map.find_exn t kb in
  a.used <> 0 && a.used <= b.avail
;;

let parse s =
  let open Parsing_util in
  let open Angstrom in
  let skip_line = skip_while (fun c -> Char.(c <> '\n')) *> end_of_line in
  let blank = take_while1 Char.is_whitespace in
  let data = number <* char 'T' in
  let node =
    let+ x = string "/dev/grid/node-x" *> number
    and+ y = string "-y" *> number
    and+ _size = blank *> data
    and+ used = blank *> data
    and+ avail = blank *> data
    and+ _use_percent = blank *> number <* string "%" in
    (x, y), { used; avail }
  in
  parse_using (skip_line *> skip_line *> many1 (node <* end_of_line)) s
  |> Map.of_alist_exn (module Vec)
;;

let pairs l ~equal =
  let open List.Let_syntax in
  let%bind a = l in
  let%bind b = l in
  if not (equal a b) then [ a, b ] else []
;;

let f1 s =
  let t = parse s in
  t |> Map.keys |> pairs ~equal:[%equal: Vec.t] |> List.count ~f:(is_viable_pair t)
;;

let dims l =
  let xmin, xmax =
    List.fold l ~init:(Int.min_value, Int.min_value) ~f:(fun (xmax, ymax) (x, y) ->
      Int.max xmax x, Int.max ymax y)
  in
  xmin + 1, xmax + 1
;;

let classify = function
  | { used = 0; _ } -> '_'
  | { used; _ } when used > 100 -> '#'
  | _ -> '.'
;;

let display t =
  let keys = Map.keys t in
  let dimx, dimy = dims keys in
  let a = Array.make_matrix ~dimx ~dimy 'x' in
  List.iter keys ~f:(fun pos ->
    let node = Map.find_exn t pos in
    let x, y = pos in
    a.(x).(y) <- classify node);
  a.(dimx - 1).(0) <- 'G';
  for y = 0 to dimy - 1 do
    for x = 0 to dimx - 1 do
      printf "%c" a.(x).(y)
    done;
    printf "\n"
  done;
  dimx
;;

let solve t =
  let dimx = display t in
  let move_empty_left = 5 in
  let move_empty_up = 34 in
  let move_empty_right = 5 in
  let shift_right = 5 in
  let shift_right_last = 1 in
  move_empty_left
  + move_empty_up
  + move_empty_right
  + ((dimx - 2) * shift_right)
  + shift_right_last
;;

let f2 s = parse s |> solve
let run () = Run.run ~f1 ~f2 Day22_input.data
