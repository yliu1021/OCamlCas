open Core_kernel
module KeywordSet = Set.M (Keyword)

type t = { keywords : KeywordSet.t }

let keywords =
  Set.of_list
    (module Keyword)
    [ "sin", Function (* trig *)
    ; "cos", Function
    ; "tan", Function
    ; "sec", Function
    ; "csc", Function
    ; "cot", Function
    ; "asin", Function (* inverse trig *)
    ; "acos", Function
    ; "atan", Function
    ; "asec", Function
    ; "acsc", Function
    ; "acot", Function
    ; "sinh", Function (* hyperbolic trig *)
    ; "cosh", Function
    ; "tanh", Function
    ; "sech", Function
    ; "csch", Function
    ; "coth", Function
    ; "asinh", Function (* inverse hyperbolic trig *)
    ; "acosh", Function
    ; "atanh", Function
    ; "asech", Function
    ; "acsch", Function
    ; "acoth", Function
    ; "log", Function (* exp/log *)
    ; "ln", Function
    ; "exp", Function
    ; "sqrt", Function (* roots *)
    ; "cbrt", Function
    ; "max", Function (* max/min *)
    ; "min", Function
    ; "e", Constant (* constants *)
    ; "pi", Constant
    ; "i", Constant
    ]
;;

let init = { keywords }
let keywords x = Set.map (module String) ~f:(fun (name, _) -> name) x.keywords
