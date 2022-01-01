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
val comma : t -> t -> t
val equate : t -> t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val exp : t -> t -> t
val neg : t -> t
val ( #> ) : string -> t -> t
val ( @ ) : t -> t -> t
val ( = ) : t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( ** ) : t -> t -> t
val ( -! ) : t -> t
val to_comma_list : t -> t list
val to_string : t -> string
