type token_type =
  | Value of string
  | LeftParenthesis
  | RightParenthesis
  | Comma
  | Equals
  | Plus
  | Minus
  | Multiply
  | Divide
  | Exponentiate
  | Negate
[@@deriving sexp]

type t =
  { token : token_type
  ; pos : int
  }
[@@deriving sexp]

val tokenize : string -> (t list, int) result
val equal : token_type -> token_type -> bool
