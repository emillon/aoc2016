module Register : sig
  type t =
    | A
    | B
    | C
    | D
end

type t

val parse : string -> t
val exec : t -> int
val set : t -> reg:Register.t -> data:int -> t
val optimize : t -> t
