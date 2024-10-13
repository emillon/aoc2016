type t =
  { paths : Set.M(Vec).t
  ; points : Vec.t Map.M(Char).t
  }

let parse s =
  let points = ref (Map.empty (module Char)) in
  let rows = String.split_lines s in
  let paths_l =
    List.concat_mapi rows ~f:(fun j row ->
      String.to_list row
      |> List.concat_mapi ~f:(fun i c ->
        let pos = i, j in
        match c with
        | '#' -> []
        | '0' .. '9' ->
          points := Map.add_exn !points ~key:c ~data:pos;
          [ pos ]
        | '.' -> [ pos ]
        | _ -> raise_s [%message "parse" (c : char) (pos : int * int)]))
  in
  { paths = Set.of_list (module Vec) paths_l; points = !points }
;;

let is_valid m pos = Set.mem m.paths pos
let next pos = Vec.neighbours4 pos
let find_point m n = Map.find_exn m.points n

let distance m start stop =
  let q = Queue.create () in
  Queue.enqueue q (start, 0);
  let seen = Hash_set.create (module Vec) in
  let is_seen pos = Hash_set.mem seen pos in
  let mark_as_seen pos = Hash_set.add seen pos in
  let exception Found of int in
  try
    while not (Queue.is_empty q) do
      let pos, n = Queue.dequeue_exn q in
      if is_valid m pos && not (is_seen pos)
      then (
        if Vec.equal pos stop then raise (Found n);
        mark_as_seen pos;
        next pos |> List.map ~f:(fun p -> p, n + 1) |> Queue.enqueue_all q)
    done;
    assert false
  with
  | Found n -> n
;;

let sample =
  String.concat_lines
    [ "###########"; "#0.1.....2#"; "#.#######.#"; "#4.......3#"; "###########" ]
;;

let%expect_test "distance" =
  let m = parse sample in
  let test a b = distance m (find_point m a) (find_point m b) |> printf "%d" in
  test '0' '4';
  [%expect {| 2 |}];
  test '4' '1';
  [%expect {| 4 |}];
  test '1' '2';
  [%expect {| 6 |}];
  test '2' '3';
  [%expect {| 2 |}]
;;

module Key = struct
  module T = struct
    type t = char * char [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let distance_map m =
  let points = Map.to_alist m.points in
  Map.of_alist_exn
    (module Key)
    (let open List.Let_syntax in
     let%bind a, pa = points in
     let%bind b, pb = points in
     let%map () = Algo.guard Char.(a < b) in
     (a, b), distance m pa pb)
;;

let find_in_distance_map m a b =
  let k = if Char.(a < b) then a, b else b, a in
  Map.find_exn m k
;;

let shortest m ~include_trip_back =
  let dm = distance_map m in
  let all_points = Map.keys m.points in
  let nonzero = List.filter all_points ~f:(fun c -> not (Char.equal c '0')) in
  Algo.permutations nonzero
  |> List.map ~f:(fun l ->
    let path = ('0' :: l) @ if include_trip_back then [ '0' ] else [] in
    let legs = Algo.legs path in
    List.fold legs ~init:0 ~f:(fun n (start, stop) ->
      n + find_in_distance_map dm start stop))
  |> List.min_elt ~compare:[%compare: int]
  |> Option.value_exn
;;

let%expect_test "shortest" =
  parse sample |> shortest ~include_trip_back:false |> printf "%d";
  [%expect {| 14 |}]
;;

let f1 s = parse s |> shortest ~include_trip_back:false
let f2 s = parse s |> shortest ~include_trip_back:true
let run () = Run.run ~f1 ~f2 Day24_input.data
