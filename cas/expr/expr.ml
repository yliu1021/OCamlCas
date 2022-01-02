open Base

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

let node x = Node x
let apply fn x = Application (fn, x)
let comma a b = Comma (a, b)
let equate a b = Equation (a, b)
let add a b = Addition (a, b)
let sub a b = Subtraction (a, b)
let mul a b = Multiplication (a, b)
let div a b = Division (a, b)
let exp a b = Exponentiation (a, b)
let neg a = Negation a
let ( #> ) = apply
let ( @ ) = comma
let ( = ) = equate
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div
let ( ** ) = exp
let ( -! ) = neg

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
    | Application (fn, x) -> fn ^ parenthesize @@ to_string x, 100
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
    | Equation (l, r) -> bin_op 20 `Right " = " l r
    | Addition (l, r) -> bin_op 30 `Left " + " l r
    | Subtraction (l, r) -> bin_op 30 `Left " - " l r
    | Multiplication (l, r) -> bin_op 40 `Left " * " l r
    | Division (l, r) -> bin_op 40 `Left " / " l r
    | Exponentiation (l, r) -> bin_op 50 `Right "^" l r
    | Negation c ->
      let negate_precedence = 60 in
      let cstr_raw, cprec = to_string_and_prec c in
      let cstr = if cprec < negate_precedence then parenthesize cstr_raw else cstr_raw in
      "-" ^ cstr, negate_precedence
  in
  let str, _ = to_string_and_prec x in
  str
;;
