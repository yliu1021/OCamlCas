type t =
  | Leaf of Tokenizer.t
  | PrefixOp of
      { token : Tokenizer.t
      ; child : t
      }
  | InfixOp of
      { token : Tokenizer.t
      ; left : t
      ; right : t
      }
[@@deriving sexp, equal]

val parse : Tokenizer.t list -> t option
