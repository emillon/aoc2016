let sample =
  String.concat_lines
    [ "The first floor contains a hydrogen-compatible microchip and a lithium-compatible \
       microchip."
    ; "The second floor contains a hydrogen generator."
    ; "The third floor contains a lithium generator."
    ; "The fourth floor contains nothing relevant."
    ]
;;

module Element = String

module Item = struct
  module T = struct
    type t =
      | Microchip of Element.t
      | Generator of Element.t
    [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let element = function
    | Microchip e -> e
    | Generator e -> e
  ;;
end

module Floor = struct
  module T = struct
    type t =
      | F1
      | F2
      | F3
      | F4
    [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let adjacent = function
    | F1 -> [ F2 ]
    | F2 -> [ F1; F3 ]
    | F3 -> [ F2; F4 ]
    | F4 -> [ F3 ]
  ;;

  let all = [ F1; F2; F3; F4 ]
end

type 'a element_info =
  { microchip : 'a
  ; generator : 'a
  }
[@@deriving compare, hash, sexp]

type t =
  { elevator : Floor.t
  ; floors : Set.M(Item).t Map.M(Floor).t
  }
[@@deriving compare, hash, sexp]

let parse s =
  let open Parsing_util in
  let open Angstrom in
  let element = word in
  let item =
    choice
      [ (let+ element = string "a " *> element <* string " generator" in
         Item.Generator element)
      ; (let+ element = string "a " *> element <* string "-compatible microchip" in
         Item.Microchip element)
      ]
  in
  let item_list =
    choice
      [ sep_by1 (choice [ string ", and "; string ", "; string " and " ]) item
      ; string "nothing relevant" *> return []
      ]
  in
  let floor = enum [ "first", Floor.F1; "second", F2; "third", F3; "fourth", F4 ] in
  let line =
    let+ floor = string "The " *> floor <* string " floor contains "
    and+ items = item_list <* string "." in
    floor, items
  in
  let items = parse_lines_using line s in
  let floors =
    items
    |> List.map ~f:(fun (f, l) -> f, Set.of_list (module Item) l)
    |> Map.of_alist_exn (module Floor)
  in
  { elevator = F1; floors }
;;

let elements_in t =
  Map.fold
    t.floors
    ~init:(Set.empty (module Element))
    ~f:(fun ~key:_ ~data:floor acc ->
      Set.map (module Element) floor ~f:Item.element |> Set.union acc)
  |> Set.to_list
;;

let rec subsets2 = function
  | [] -> []
  | x :: xs -> [ [ x ] ] @ List.map xs ~f:(fun y -> [ x; y ]) @ subsets2 xs
;;

let%expect_test "subsets2" =
  let test l = subsets2 l |> [%sexp_of: int list list] |> print_s in
  test [];
  [%expect {| () |}];
  test [ 1 ];
  [%expect {| ((1)) |}];
  test [ 1; 2 ];
  [%expect {| ((1) (1 2) (2)) |}];
  test [ 1; 2; 3 ];
  [%expect {| ((1) (1 2) (1 3) (2) (2 3) (3)) |}]
;;

let movable_items_on_elevator_floor t =
  Map.find_exn t.floors t.elevator |> Set.to_list |> subsets2
;;

let move_elevator t elevator = { t with elevator }

let has_no_isolated_chips_on_floor floor =
  Set.for_all floor ~f:(function
    | Item.Generator _ -> true
    | Item.Microchip e -> Set.mem floor (Item.Generator e))
;;

let has_no_generators_on_floor floor =
  Set.for_all floor ~f:(function
    | Item.Generator _ -> false
    | Microchip _ -> true)
;;

let is_valid_floor floor =
  has_no_isolated_chips_on_floor floor || has_no_generators_on_floor floor
;;

let move_item_to t f ~prev_floor item =
  let prev_floor_ = Map.find_exn t.floors prev_floor in
  let new_prev_floor = Set.remove prev_floor_ item in
  let current_floor = Map.find_exn t.floors f in
  let new_current_floor = Set.add current_floor item in
  let new_floors1 = Map.set t.floors ~key:prev_floor ~data:new_prev_floor in
  let new_floors2 = Map.set new_floors1 ~key:f ~data:new_current_floor in
  { t with floors = new_floors2 }
;;

let move_to t f items =
  let prev_floor = t.elevator in
  List.fold items ~init:(move_elevator t f) ~f:(fun t item ->
    move_item_to t f item ~prev_floor)
;;

let next t =
  let open List.Let_syntax in
  let%bind moved_items = movable_items_on_elevator_floor t in
  let%map f = Floor.adjacent t.elevator in
  move_to t f moved_items
;;

let is_valid t =
  List.for_all Floor.all ~f:(fun f ->
    let floor = Map.find_exn t.floors f in
    is_valid_floor floor)
;;

let is_winning t =
  List.for_all [ Floor.F1; F2; F3 ] ~f:(fun f ->
    let floor = Map.find_exn t.floors f in
    Set.is_empty floor)
;;

module Summary = struct
  module T = struct
    type t =
      { elevator : Floor.t
      ; pairs : (Floor.t * Floor.t) list
      }
    [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let find_item t i =
    Map.to_alist t.floors
    |> List.find_map_exn ~f:(fun (f, s) -> Option.some_if (Set.mem s i) f)
  ;;

  let of_ t =
    let pairs =
      elements_in t
      |> List.map ~f:(fun e ->
        let chip = find_item t (Item.Microchip e) in
        let rtg = find_item t (Item.Generator e) in
        chip, rtg)
      |> List.sort ~compare:[%compare: Floor.t * Floor.t]
    in
    { elevator = t.elevator; pairs }
  ;;
end

let min_steps t =
  let exception Found of int in
  let seen = Hash_set.create (module Summary) in
  let is_seen t = Hash_set.mem seen (Summary.of_ t) in
  let mark_seen t = Hash_set.add seen (Summary.of_ t) in
  let q = Queue.create () in
  Queue.enqueue q (t, 0);
  try
    while not (Queue.is_empty q) do
      let t, steps = Queue.dequeue_exn q in
      if (not (is_seen t)) && is_valid t
      then (
        if is_winning t then raise (Found steps);
        mark_seen t;
        next t |> List.map ~f:(fun t -> t, steps + 1) |> Queue.enqueue_all q)
    done;
    -1
  with
  | Found n -> n
;;

let%expect_test "min_steps" =
  parse sample |> min_steps |> printf "%d";
  [%expect {| 11 |}]
;;

let f1 s = parse s |> min_steps

let add_missing_element element t =
  { t with
    floors =
      Map.update t.floors F1 ~f:(fun so ->
        let s = Option.value_exn so in
        let s = Set.add s (Item.Microchip element) in
        let s = Set.add s (Item.Generator element) in
        s)
  }
;;

let add_missing_stuff t =
  t |> add_missing_element "elerium" |> add_missing_element "dilithium"
;;

let f2 s = parse s |> add_missing_stuff |> min_steps
let run () = Run.run ~f1 ~f2 Day11_input.data
