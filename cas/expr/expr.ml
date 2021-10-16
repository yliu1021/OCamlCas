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

let rec of_parse_tree =
  let ( >>== ) a f =
    match a with
    | Result.Error e -> Result.Error e
    | Result.Ok x -> Result.Ok (f x)
  in
  let ( >>>= ) a f =
    match a with
    | Result.Ok x, Result.Ok y -> Result.Ok (f x y)
    | Result.Error e, _ -> Result.Error e
    | _, Result.Error e -> Result.Error e
  in
  let open Result in
  function
  | Parser.Leaf x -> Ok (Node Tokenizer.(x.value))
  | Parser.PrefixOp { token; child } ->
    (match Tokenizer.(token.token) with
    | Negate -> of_parse_tree child >>== ( -/ )
    | Function -> of_parse_tree child >>== ( @@| ) Tokenizer.(token.value)
    | _ -> Error Tokenizer.(token.pos))
  | Parser.InfixOp { token; left; right } ->
    (match Tokenizer.(token.token) with
    | Comma -> (of_parse_tree left, of_parse_tree right) >>>= ( @| )
    | Equals -> (of_parse_tree left, of_parse_tree right) >>>= ( =| )
    | Plus -> (of_parse_tree left, of_parse_tree right) >>>= ( +| )
    | Minus -> (of_parse_tree left, of_parse_tree right) >>>= ( -| )
    | Multiply -> (of_parse_tree left, of_parse_tree right) >>>= ( *| )
    | Divide -> (of_parse_tree left, of_parse_tree right) >>>= ( /| )
    | Exponentiate -> (of_parse_tree left, of_parse_tree right) >>>= ( **| )
    | _ -> Error Tokenizer.(token.pos))
;;
