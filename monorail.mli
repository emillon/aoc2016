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
val step : t -> t option
val set : t -> reg:Register.t -> data:int -> t
val optimize : t -> t
val set_out_handler : t -> f:(int -> unit) -> t
