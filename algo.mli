val sum : int list -> int
val product : int list -> int
val permutations : 'a list -> 'a list list
val sublists : 'a list -> 'a list list
val legs : 'a list -> ('a * 'a) list
val iter_bytes : f:(bytes -> bool) -> start:string -> min:char -> max:char -> string
val guard : bool -> unit list
