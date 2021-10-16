open Base

type token_type =
  | Function
  | Constant
  | Variable
  | LeftParenthesis
  | RightParenthesis
  | Comma
  | Equals
  | Plus
  | Minus
  | Multiply
  | Divide
  | Exponentiate
  | Negate
[@@deriving sexp, equal]

type t =
  { token : token_type
  ; pos : int
  ; value : string
  }
[@@deriving sexp, equal]

let minus_or_negate prev_token =
  match prev_token with
  | None -> Negate
  | Some token ->
    (match token with
    | Function | Constant | Variable -> Minus
    | LeftParenthesis -> Negate
    | RightParenthesis -> Minus
    | Comma | Equals -> Negate
    | Plus | Minus | Multiply | Divide | Exponentiate | Negate -> Negate)
;;

let tokenize_char_value =
  let rec tok_chars = function
    | (('a' .. 'z' | 'A' .. 'Z') as ch) :: rem_chars ->
      let parsed_chars, rem = tok_chars rem_chars in
      ch :: parsed_chars, rem
    | chars -> [], chars
  in
  tok_chars
;;

let tokenize_number_value =
  let rec tok_numbers pos seen_decimal = function
    | ('0' .. '9' as digit) :: rem_chars ->
      (match tok_numbers (pos + 1) seen_decimal rem_chars with
      | None -> None
      | Some (parsed_number, rem) -> Some (digit :: parsed_number, rem))
    | '.' :: rem_chars ->
      if seen_decimal
      then None
      else (
        match tok_numbers (pos + 1) true rem_chars with
        | None -> None
        | Some (parsed_number, rem) -> Some ('.' :: parsed_number, rem))
    | chars -> Some ([], chars)
  in
  tok_numbers 0 false
;;

let get_next_token prev_token chars =
  match chars with
  | '(' :: rem_chars -> Some ((LeftParenthesis, "("), rem_chars)
  | ')' :: rem_chars -> Some ((RightParenthesis, ")"), rem_chars)
  | ',' :: rem_chars -> Some ((Comma, ","), rem_chars)
  | '=' :: rem_chars -> Some ((Equals, "="), rem_chars)
  | '+' :: rem_chars -> Some ((Plus, "+"), rem_chars)
  | '-' :: rem_chars -> Some ((minus_or_negate prev_token, "-"), rem_chars)
  | '*' :: '*' :: rem_chars -> Some ((Exponentiate, "**"), rem_chars)
  | '*' :: rem_chars -> Some ((Multiply, "*"), rem_chars)
  | '/' :: rem_chars -> Some ((Divide, "/"), rem_chars)
  | '^' :: rem_chars -> Some ((Exponentiate, "^"), rem_chars)
  | ('a' .. 'z' | 'A' .. 'Z') :: _ ->
    let parsed_chars, rem_chars = tokenize_char_value chars in
    let parsed_str = String.of_char_list parsed_chars in
    Some ((Function, parsed_str), rem_chars)
  | ('0' .. '9' | '.') :: _ ->
    (match tokenize_number_value chars with
    | None -> None
    | Some (parsed_digits, rem_chars) ->
      let parsed_num = String.of_char_list parsed_digits in
      Some ((Constant, parsed_num), rem_chars))
  | _ -> None
;;

let rec resolve_keywords repl_state = function
  | [] -> []
  | tok :: rem_toks ->
    let rest = resolve_keywords repl_state rem_toks in
    (match tok with
    | { token = Function; pos; value } ->
      (match Repl_state.keyword_type repl_state value with
      | Some Repl_state.Constant -> { token = Constant; pos; value } :: rest
      | Some Repl_state.Function -> { token = Function; pos; value } :: rest
      | None ->
        let split =
          List.mapi
            ~f:(fun i a -> { token = Variable; pos = pos + i; value = Char.to_string a })
            (String.to_list value)
        in
        split @ rest)
    | _ -> tok :: rest)
;;

let tokenize ?(repl_state = Repl_state.init) input_str =
  let rec state_machine prev_token pos = function
    | [] -> Result.Ok []
    | (' ' | '\t') :: chars -> state_machine prev_token (pos + 1) chars
    | chars ->
      (match get_next_token prev_token chars with
      | None -> Result.Error pos
      | Some ((token, value), rem_chars) ->
        (match state_machine (Some token) (pos + String.length value) rem_chars with
        | Result.Error pos -> Result.Error pos
        | Result.Ok lst -> Result.Ok ({ token; pos; value } :: lst)))
  in
  let open Result in
  String.to_list input_str |> state_machine None 0 >>| resolve_keywords repl_state
;;
