open Base
module StringSet = Set.M (String)

type t = { keywords : StringSet.t }

let keywords =
  Set.of_list
    (module String)
    [ "sin" (* trig *)
    ; "cos"
    ; "tan"
    ; "sec"
    ; "csc"
    ; "cot"
    ; "asin" (* inverse trig *)
    ; "acos"
    ; "atan"
    ; "asec"
    ; "acsc"
    ; "acot"
    ; "sinh" (* hyperbolic trig *)
    ; "cosh"
    ; "tanh"
    ; "sech"
    ; "csch"
    ; "coth"
    ; "asinh" (* inverse hyperbolic trig *)
    ; "acosh"
    ; "atanh"
    ; "asech"
    ; "acsch"
    ; "acoth"
    ; "log" (* exp/log *)
    ; "ln"
    ; "exp"
    ; "sqrt" (* roots *)
    ; "cbrt"
    ; "max" (* max/min *)
    ; "min"
    ]
;;

let init = { keywords }
let keywords x = x.keywords
