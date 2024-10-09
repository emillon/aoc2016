let is_valid (x, y) = x >= 0 && y >= 0

let is_open_space fav (x, y) =
  let bits = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + fav |> Int.popcount in
  bits % 2 = 0
;;

let next pos fav = Vec.neighbours4 pos |> List.filter ~f:(is_open_space fav)

let p1 fav target =
  let exception Found of int in
  let q = Queue.create () in
  let seen = Hash_set.create (module Vec) in
  Queue.enqueue q ((1, 1), 0);
  try
    while not (Queue.is_empty q) do
      let pos, steps = Queue.dequeue_exn q in
      if is_valid pos && not (Hash_set.mem seen pos)
      then (
        if Vec.equal pos target then raise (Found steps);
        Hash_set.add seen pos;
        next pos fav |> List.map ~f:(fun p -> p, steps + 1) |> Queue.enqueue_all q)
    done;
    -1
  with
  | Found n -> n
;;

let%expect_test "p1" =
  p1 10 (7, 4) |> printf "%d";
  [%expect {| 11 |}]
;;

let parse s = String.strip s |> Int.of_string
let f1 s = p1 (parse s) (31, 39)

let p2 fav =
  let q = Queue.create () in
  let seen = Hash_set.create (module Vec) in
  Queue.enqueue q ((1, 1), 0);
  while not (Queue.is_empty q) do
    let pos, steps = Queue.dequeue_exn q in
    if is_valid pos && steps <= 50 && not (Hash_set.mem seen pos)
    then (
      Hash_set.add seen pos;
      next pos fav |> List.map ~f:(fun p -> p, steps + 1) |> Queue.enqueue_all q)
  done;
  Hash_set.length seen
;;

let f2 s = p2 (parse s)
let run () = Run.run ~f1 ~f2 Day13_input.data
