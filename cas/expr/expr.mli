type t =
  | Node of string
  | Application of (string * t)
  | Comma of (t * t)
  | Equation of (t * t)
  | Addition of (t * t)
  | Subtraction of (t * t)
  | Multiplication of (t * t)
  | Division of (t * t)
  | Exponentiation of (t * t)
  | Negation of t
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
