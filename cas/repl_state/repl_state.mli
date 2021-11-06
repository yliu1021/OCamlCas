type t

type keyword_type =
  | Function
  | Constant

val init : t
val keywords : t -> (string, Base.String.comparator_witness) Base.Set.t
val keyword_type : t -> string -> keyword_type option
