open Base
open Core

type keyword_type =
  | Function
  | Variable
  | Constant
[@@deriving compare, sexp]

module T = struct
  type t = string * keyword_type [@@deriving compare, sexp]
end

include T

include Base.Comparator.Make (T)
