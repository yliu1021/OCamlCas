open Base
open Expr

let rec to_float = function
  | Add (x, y) -> to_float x +. to_float y
  | Subtract (x, y) -> to_float x -. to_float y
  | Multiply (x, y) -> to_float x *. to_float y
  | Divide (x, y) -> to_float x /. to_float y
  | Exponentiate (x, y) -> to_float x **. to_float y
  | Negate x -> to_float x |> Float.neg
  | Node "e" -> Float.exp 1.
  | Node "pi" -> Float.pi
  | Node x ->
    (try Float.of_string x with
    | _ -> Float.nan)
  | Apply ("sqrt", x) -> Float.sqrt @@ to_float x
  | Apply ("cbrt", x) -> to_float x **. (1. /. 3.)
  | Apply ("log", x) -> Float.log10 @@ to_float x
  | Apply ("ln", x) -> Float.log @@ to_float x
  | Apply ("exp", x) -> Float.exp @@ to_float x
  | Apply ("max", x) ->
    (match List.map ~f:to_float (Expr.to_comma_list x) with
    | [] -> Float.nan
    | num :: rem -> List.fold rem ~init:num ~f:Float.max)
  | Apply ("min", x) ->
    (match List.map ~f:to_float (Expr.to_comma_list x) with
    | [] -> Float.nan
    | num :: rem -> List.fold rem ~init:num ~f:Float.min)
  | Apply ("abs", x) -> Float.abs @@ to_float x
  | Apply ("sgn", x) ->
    let y = to_float x in
    if Float.(y < 0.) then -1. else if Float.(y > 0.) then 1. else 0.
  | Apply ("sin", x) -> Float.sin @@ to_float x
  | Apply ("cos", x) -> Float.cos @@ to_float x
  | Apply ("tan", x) -> Float.tan @@ to_float x
  | Apply ("sec", x) -> 1. /. (Float.cos @@ to_float x)
  | Apply ("csc", x) -> 1. /. (Float.sin @@ to_float x)
  | Apply ("cot", x) -> 1. /. (Float.tan @@ to_float x)
  | Apply ("asin", x) -> Float.asin @@ to_float x
  | Apply ("acos", x) -> Float.acos @@ to_float x
  | Apply ("atan", x) -> Float.atan @@ to_float x
  | Apply ("asec", x) -> Float.acos (1. /. to_float x)
  | Apply ("acsc", x) -> Float.asin (1. /. to_float x)
  | Apply ("acot", x) -> Float.atan (1. /. to_float x)
  | Apply ("sinh", x) ->
    let y = to_float x in
    (Float.exp y -. Float.exp (Float.neg y)) /. 2.
  | Apply ("cosh", x) ->
    let y = to_float x in
    (Float.exp y +. Float.exp (Float.neg y)) /. 2.
  | Apply ("tanh", x) ->
    let y = to_float x in
    (Float.exp y -. Float.exp (Float.neg y)) /. (Float.exp y +. Float.exp (Float.neg y))
  | Apply ("sech", x) ->
    let y = to_float x in
    2. /. (Float.exp y +. Float.exp (Float.neg y))
  | Apply ("csch", x) ->
    let y = to_float x in
    2. /. (Float.exp y -. Float.exp (Float.neg y))
  | Apply ("coth", x) ->
    let y = to_float x in
    (Float.exp y +. Float.exp (Float.neg y)) /. (Float.exp y -. Float.exp (Float.neg y))
  | Apply ("asinh", x) ->
    let y = to_float x in
    Float.log (y +. (((y **. 2.) +. 1.) **. 0.5))
  | Apply ("acosh", x) ->
    let y = to_float x in
    Float.log (y +. (((y **. 2.) -. 1.) **. 0.5))
  | Apply ("atanh", x) ->
    let y = to_float x in
    0.5 *. Float.log ((1. +. y) /. (1. -. y))
  | Apply ("asech", x) ->
    let y = to_float x in
    Float.log ((1. +. ((1. -. (y **. 2.)) **. 0.5)) /. y)
  | Apply ("acsch", x) ->
    let y = to_float x in
    Float.log ((1. +. ((1. +. (y **. 2.)) **. 0.5)) /. Float.abs y)
  | Apply ("acoth", x) ->
    let y = to_float x in
    0.5 *. Float.log ((y +. 1.) /. (y -. 1.))
  | _ -> Float.nan
;;
