open Base
open Expr

let rec to_float = function
  | Addition (x, y) -> to_float x +. to_float y
  | Subtraction (x, y) -> to_float x -. to_float y
  | Multiplication (x, y) -> to_float x *. to_float y
  | Division (x, y) -> to_float x /. to_float y
  | Exponentiation (x, y) -> to_float x **. to_float y
  | Negation x -> to_float x |> Float.neg
  | Node "e" -> Float.exp 1.
  | Node "pi" -> Float.pi
  | Node x ->
    (try Float.of_string x with
    | _ -> Float.nan)
  | Application ("sqrt", x) -> Float.sqrt @@ to_float x
  | Application ("cbrt", x) -> to_float x **. (1. /. 3.)
  | Application ("log", x) -> Float.log10 @@ to_float x
  | Application ("ln", x) -> Float.log @@ to_float x
  | Application ("exp", x) -> Float.exp @@ to_float x
  | Application ("max", x) ->
    (match List.map ~f:to_float (Expr.to_comma_list x) with
    | [] -> Float.nan
    | num :: rem -> List.fold rem ~init:num ~f:Float.max)
  | Application ("min", x) ->
    (match List.map ~f:to_float (Expr.to_comma_list x) with
    | [] -> Float.nan
    | num :: rem -> List.fold rem ~init:num ~f:Float.min)
  | Application ("abs", x) -> Float.abs @@ to_float x
  | Application ("sgn", x) ->
    let y = to_float x in
    if Float.(y < 0.) then -1. else if Float.(y > 0.) then 1. else 0.
  | Application ("sin", x) -> Float.sin @@ to_float x
  | Application ("cos", x) -> Float.cos @@ to_float x
  | Application ("tan", x) -> Float.tan @@ to_float x
  | Application ("sec", x) -> 1. /. (Float.cos @@ to_float x)
  | Application ("csc", x) -> 1. /. (Float.sin @@ to_float x)
  | Application ("cot", x) -> 1. /. (Float.tan @@ to_float x)
  | Application ("asin", x) -> Float.asin @@ to_float x
  | Application ("acos", x) -> Float.acos @@ to_float x
  | Application ("atan", x) -> Float.atan @@ to_float x
  | Application ("asec", x) -> Float.acos (1. /. to_float x)
  | Application ("acsc", x) -> Float.asin (1. /. to_float x)
  | Application ("acot", x) -> Float.atan (1. /. to_float x)
  | Application ("sinh", x) ->
    let y = to_float x in
    (Float.exp y -. Float.exp (Float.neg y)) /. 2.
  | Application ("cosh", x) ->
    let y = to_float x in
    (Float.exp y +. Float.exp (Float.neg y)) /. 2.
  | Application ("tanh", x) ->
    let y = to_float x in
    (Float.exp y -. Float.exp (Float.neg y)) /. (Float.exp y +. Float.exp (Float.neg y))
  | Application ("sech", x) ->
    let y = to_float x in
    2. /. (Float.exp y +. Float.exp (Float.neg y))
  | Application ("csch", x) ->
    let y = to_float x in
    2. /. (Float.exp y -. Float.exp (Float.neg y))
  | Application ("coth", x) ->
    let y = to_float x in
    (Float.exp y +. Float.exp (Float.neg y)) /. (Float.exp y -. Float.exp (Float.neg y))
  | Application ("asinh", x) ->
    let y = to_float x in
    Float.log (y +. (((y **. 2.) +. 1.) **. 0.5))
  | Application ("acosh", x) ->
    let y = to_float x in
    Float.log (y +. (((y **. 2.) -. 1.) **. 0.5))
  | Application ("atanh", x) ->
    let y = to_float x in
    0.5 *. Float.log ((1. +. y) /. (1. -. y))
  | Application ("asech", x) ->
    let y = to_float x in
    Float.log ((1. +. ((1. -. (y **. 2.)) **. 0.5)) /. y)
  | Application ("acsch", x) ->
    let y = to_float x in
    Float.log ((1. +. ((1. +. (y **. 2.)) **. 0.5)) /. Float.abs y)
  | Application ("acoth", x) ->
    let y = to_float x in
    0.5 *. Float.log ((y +. 1.) /. (y -. 1.))
  | _ -> Float.nan
;;
