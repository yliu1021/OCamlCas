type t =
  | Node of string
  | Apply of (string * t)
  | Comma of (t * t)
  | Equate of (t * t)
  | Add of (t * t)
  | Subtract of (t * t)
  | Multiply of (t * t)
  | Divide of (t * t)
  | Exponentiate of (t * t)
  | Negate of t
[@@deriving sexp, equal]

val node : string -> t
val apply : string -> t -> t
val ( @@| ) : string -> t -> t
val ( @| ) : t -> t -> t
val ( =| ) : t -> t -> t
val ( +| ) : t -> t -> t
val ( -| ) : t -> t -> t
val ( *| ) : t -> t -> t
val ( /| ) : t -> t -> t
val ( **| ) : t -> t -> t
val ( -/ ) : t -> t
val of_parse_tree : Parser.t -> (t, int) result
