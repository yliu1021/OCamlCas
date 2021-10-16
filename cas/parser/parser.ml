open Base

type t =
  | Leaf of Tokenizer.t
  | PrefixOp of
      { token : Tokenizer.t
      ; child : t
      }
  | InfixOp of
      { token : Tokenizer.t
      ; left : t
      ; right : t
      }
[@@deriving sexp, equal]

type expr_type =
  | ExprBase
  | ExprEq
  | ExprComma
  | ExprPlusMinus
  | ExprMultDiv
  | ExprNegate
  | ExprExp
  | ExprParenthesis
  | ExprFunctionApply
  | ExprValue
  | ExprFunction
[@@deriving sexp]

type substitution =
  | Promotion of expr_type
  | InfixSubstitution of (expr_type * Tokenizer.token_type * expr_type)
  | PrefixSubstitution of (Tokenizer.token_type * expr_type)
  | ImplicitMultiply of (expr_type * expr_type)
  | ParenthesesReduce of expr_type
  | TerminalValue
  | TerminalFunction
[@@deriving sexp]

let get_substitions = function
  | ExprBase -> [ Promotion ExprEq ]
  | ExprEq ->
    [ Promotion ExprComma; InfixSubstitution (ExprComma, Tokenizer.Equals, ExprEq) ]
  | ExprComma ->
    [ Promotion ExprPlusMinus
    ; InfixSubstitution (ExprComma, Tokenizer.Comma, ExprPlusMinus)
    ]
  | ExprPlusMinus ->
    [ Promotion ExprMultDiv
    ; InfixSubstitution (ExprPlusMinus, Tokenizer.Plus, ExprMultDiv)
    ; InfixSubstitution (ExprPlusMinus, Tokenizer.Minus, ExprMultDiv)
    ]
  | ExprMultDiv ->
    [ Promotion ExprNegate
    ; InfixSubstitution (ExprMultDiv, Tokenizer.Multiply, ExprNegate)
    ; InfixSubstitution (ExprMultDiv, Tokenizer.Divide, ExprNegate)
    ; ImplicitMultiply (ExprMultDiv, ExprNegate)
    ]
  | ExprNegate ->
    [ Promotion ExprExp
    ; PrefixSubstitution (Tokenizer.Negate, ExprNegate)
    ; PrefixSubstitution (Tokenizer.Negate, ExprParenthesis)
    ]
  | ExprExp ->
    [ Promotion ExprFunctionApply
    ; Promotion ExprParenthesis
    ; InfixSubstitution (ExprFunctionApply, Tokenizer.Exponentiate, ExprExp)
    ]
  | ExprFunctionApply ->
    [ Promotion ExprValue
    ; Promotion ExprFunction
    ; Promotion ExprParenthesis
    ; PrefixSubstitution (Tokenizer.Function, ExprParenthesis)
    ]
  | ExprParenthesis -> [ ParenthesesReduce ExprBase ]
  | ExprValue -> [ TerminalValue ]
  | ExprFunction -> [ TerminalFunction ]
;;

let rec parse_as_expr expr tokens =
  let rec match_substitutions = function
    | [] -> None
    | sub :: rem_subs ->
      (match perform_sub sub tokens with
      | Some tree -> Some tree
      | None -> match_substitutions rem_subs)
  in
  get_substitions expr |> match_substitutions

and perform_sub = function
  | Promotion expr -> parse_as_expr expr
  | InfixSubstitution (expr_left, infix_token, expr_right) ->
    let rec scan rev_left right_tokens =
      match right_tokens with
      | [] -> None
      | token :: rem_right_tokens ->
        let go_next () = scan (token :: rev_left) rem_right_tokens in
        if Tokenizer.equal_token_type Tokenizer.(token.token) infix_token
        then (
          match parse_as_expr expr_right rem_right_tokens with
          | None -> go_next ()
          | Some right ->
            (match parse_as_expr expr_left (List.rev rev_left) with
            | None -> go_next ()
            | Some left -> Some (InfixOp { token; left; right })))
        else go_next ()
    in
    scan []
  | PrefixSubstitution (prefix_token, expr_child) ->
    (function
    | [] -> None
    | token :: rem_tokens ->
      if Tokenizer.equal_token_type Tokenizer.(token.token) prefix_token
      then (
        match parse_as_expr expr_child rem_tokens with
        | None -> None
        | Some child -> Some (PrefixOp { token; child }))
      else None)
  | ImplicitMultiply (expr_left, expr_right) ->
    let rec scan rev_left right_tokens =
      match rev_left, right_tokens with
      | [], [] -> None
      | [], token :: rem_right_tokens -> scan [ token ] rem_right_tokens
      | rev_left, token :: rem_right_tokens ->
        let go_next () = scan (token :: rev_left) rem_right_tokens in
        (match parse_as_expr expr_right right_tokens with
        | None -> go_next ()
        | Some right ->
          let left_tokens = List.rev rev_left in
          (match parse_as_expr expr_left left_tokens with
          | None -> go_next ()
          | Some left ->
            Some
              (InfixOp
                 { token = Tokenizer.{ token = Tokenizer.Multiply; pos = -1; value = "" }
                 ; left
                 ; right
                 })))
      | _, [] -> None
    in
    scan []
  | ParenthesesReduce expr ->
    (function
    | [] -> None
    | tok :: rem ->
      if Tokenizer.equal_token_type Tokenizer.(tok.token) Tokenizer.LeftParenthesis
      then (
        let rev_toks = List.rev rem in
        match rev_toks with
        | [] -> None
        | tok :: mid ->
          if Tokenizer.equal_token_type Tokenizer.(tok.token) Tokenizer.RightParenthesis
          then parse_as_expr expr (List.rev mid)
          else None)
      else None)
  | TerminalValue ->
    (function
    | [ token ] ->
      (match Tokenizer.(token.token) with
      | Tokenizer.Constant | Tokenizer.Variable -> Some (Leaf token)
      | _ -> None)
    | _ -> None)
  | TerminalFunction ->
    (function
    | [ token ] ->
      (match Tokenizer.(token.token) with
      | Tokenizer.Function -> Some (Leaf token)
      | _ -> None)
    | _ -> None)
;;

let parse = parse_as_expr ExprBase
