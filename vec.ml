module T = struct
  type t = int * int [@@deriving compare, sexp]
end

include T
include Comparable.Make (T)

let l1_norm (x, y) = abs x + abs y
let zero = 0, 0
let one = 1, 0
let cmul (ax, ay) (bx, by) = (ax * bx) - (ay * by), (ay * bx) + (ax * by)
let add (ax, ay) (bx, by) = ax + bx, ay + by
