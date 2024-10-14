let solve n =
  let a = Array.init n ~f:(fun i -> (i + 1) % n) in
  let count = ref n in
  let cur = ref 0 in
  while !count > 1 do
    let next = a.(!cur) in
    let next2 = a.(next) in
    a.(next) <- -1;
    a.(!cur) <- next2;
    cur := next2;
    Int.decr count
  done;
  Array.find_mapi_exn a ~f:(fun i n -> Option.some_if (n > 0) i) + 1
;;

let%expect_test "solve" =
  solve 5 |> printf "%d";
  [%expect {| 3 |}]
;;

let parse s = String.strip s |> Int.of_string
let f1 s = parse s |> solve

let solve2 n =
  let r = ref 1 in
  for i = 1 to n - 1 do
    r := (!r % i) + 1;
    if !r > (i + 1) / 2 then Int.incr r
  done;
  !r
;;

let f2 s = parse s |> solve2
let run () = Run.run ~f1 ~f2 Day19_input.data
