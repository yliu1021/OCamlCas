type token_type =
  | Function
  | Constant
  | Variable
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
[@@deriving sexp, equal]

type t =
  { token : token_type
  ; pos : int
  ; value : string
  }
[@@deriving sexp, equal]

val tokenize : ?repl_state:Repl.t -> string -> (t list, int) result
