open Core_kernel

type keyword_type =
  | Function
  | Constant

type t = { keywords : (string, keyword_type, String.comparator_witness) Map.t }

let init =
  { keywords =
      Map.of_alist_exn
        (module String)
        [ "sqrt", Function (* roots *)
        ; "cbrt", Function
        ; "log", Function (* exp/log *)
        ; "ln", Function
        ; "exp", Function
        ; "max", Function (* max/min *)
        ; "min", Function
        ; "e", Constant (* constants *)
        ; "pi", Constant
        ; "i", Constant
        ; "sin", Function (* trig *)
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
        ]
  }
;;

let keywords state = Map.key_set state.keywords

let keyword_type state name =
  match Map.find state.keywords name with
  | Some t -> Some t
  | None ->
    if String.is_empty name
    then None
    else (
      match String.get name 0 with
      | '-' | '.' | '0' .. '9' ->
        (try
           let () = ignore (Float.of_string name) in
           Some Constant
         with
        | _ -> None)
      | _ -> None)
;;
