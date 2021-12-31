open Base

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

let node x = Node x
let apply fn x = Apply (fn, x)
let ( @@| ) fn x = apply fn x
let ( @| ) a b = Comma (a, b)
let ( =| ) a b = Equate (a, b)
let ( +| ) a b = Add (a, b)
let ( -| ) a b = Subtract (a, b)
let ( *| ) a b = Multiply (a, b)
let ( /| ) a b = Divide (a, b)
let ( **| ) a b = Exponentiate (a, b)
let ( -/ ) a = Negate a

let rec to_comma_list = function
  | Comma (l, r) -> l :: to_comma_list r
  | x -> [ x ]
;;

let rec to_string x =
  let rec to_string_and_prec =
    let parenthesize x = "(" ^ x ^ ")" in
    let bin_op precedence associativity sep l r =
      let lstr_raw, lprec = to_string_and_prec l in
      let rstr_raw, rprec = to_string_and_prec r in
      let lstr, rstr =
        match associativity with
        | `Left ->
          ( (if lprec < precedence then parenthesize lstr_raw else lstr_raw)
          , if rprec <= precedence then parenthesize rstr_raw else rstr_raw )
        | `Right ->
          ( (if lprec <= precedence then parenthesize lstr_raw else lstr_raw)
          , if rprec < precedence then parenthesize rstr_raw else rstr_raw )
      in
      lstr ^ sep ^ rstr, precedence
    in
    function
    | Node x -> x, 100
    | Apply (fn, x) -> fn ^ parenthesize @@ to_string x, 100
    | Comma (l, r) ->
      let comma_precedence = 10 in
      let cs =
        Comma (l, r)
        |> to_comma_list
        |> List.map ~f:to_string_and_prec
        |> List.map ~f:(fun (cstr_raw, cprec) ->
               if cprec <= comma_precedence then parenthesize cstr_raw else cstr_raw)
        |> String.concat ~sep:", "
      in
      cs, comma_precedence
    | Equate (l, r) -> bin_op 20 `Right " = " l r
    | Add (l, r) -> bin_op 30 `Left " + " l r
    | Subtract (l, r) -> bin_op 30 `Left " - " l r
    | Multiply (l, r) -> bin_op 40 `Left " * " l r
    | Divide (l, r) -> bin_op 40 `Left " / " l r
    | Exponentiate (l, r) -> bin_op 50 `Right "^" l r
    | Negate c ->
      let negate_precedence = 60 in
      let cstr_raw, cprec = to_string_and_prec c in
      let cstr = if cprec < negate_precedence then parenthesize cstr_raw else cstr_raw in
      "-" ^ cstr, negate_precedence
  in
  let str, _ = to_string_and_prec x in
  str
;;
