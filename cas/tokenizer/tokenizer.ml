open Base

type token_type =
  | Value of string
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
[@@deriving sexp]

type t =
  { token : token_type
  ; pos : int
  }
[@@deriving sexp]

let minus_or_negate prev_token =
  match prev_token with
  | None -> Negate
  | Some token ->
    (match token with
    | Value _ -> Minus
    | LeftParenthesis -> Negate
    | RightParenthesis -> Minus
    | Comma | Equals -> Negate
    | Plus | Minus | Multiply | Divide | Exponentiate | Negate -> Negate)
;;

let tokenize_char_value =
  let rec tok_chars pos = function
    | (('a' .. 'z' | 'A' .. 'Z') as ch) :: rem_chars ->
      let parsed_chars, num_chars, rem = tok_chars (pos + 1) rem_chars in
      ch :: parsed_chars, num_chars + 1, rem
    | chars -> [], 0, chars
  in
  tok_chars 0
;;

let tokenize_number_value =
  let rec tok_numbers pos seen_decimal = function
    | ('0' .. '9' as digit) :: rem_chars ->
      (match tok_numbers (pos + 1) seen_decimal rem_chars with
      | None -> None
      | Some (parsed_number, num_digits, rem) ->
        Some (digit :: parsed_number, num_digits + 1, rem))
    | '.' :: rem_chars ->
      if seen_decimal
      then None
      else (
        match tok_numbers (pos + 1) true rem_chars with
        | None -> None
        | Some (parsed_number, num_digits, rem) ->
          Some ('.' :: parsed_number, num_digits + 1, rem))
    | chars -> Some ([], 0, chars)
  in
  tok_numbers 0 false
;;

let get_next_token prev_token chars =
  match chars with
  | '(' :: rem_chars -> Some ((LeftParenthesis, 1), rem_chars)
  | ')' :: rem_chars -> Some ((RightParenthesis, 1), rem_chars)
  | ',' :: rem_chars -> Some ((Comma, 1), rem_chars)
  | '=' :: rem_chars -> Some ((Equals, 1), rem_chars)
  | '+' :: rem_chars -> Some ((Plus, 1), rem_chars)
  | '-' :: rem_chars -> Some ((minus_or_negate prev_token, 1), rem_chars)
  | '*' :: '*' :: rem_chars -> Some ((Exponentiate, 2), rem_chars)
  | '*' :: rem_chars -> Some ((Multiply, 1), rem_chars)
  | '/' :: rem_chars -> Some ((Divide, 1), rem_chars)
  | '^' :: rem_chars -> Some ((Exponentiate, 1), rem_chars)
  | ('a' .. 'z' | 'A' .. 'Z') :: _ ->
    let parsed_chars, num_chars, rem_chars = tokenize_char_value chars in
    let token = Value (String.of_char_list parsed_chars) in
    Some ((token, num_chars), rem_chars)
  | ('0' .. '9' | '.') :: _ ->
    (match tokenize_number_value chars with
    | None -> None
    | Some (parsed_digits, num_digits, rem_chars) ->
      let token = Value (String.of_char_list parsed_digits) in
      Some ((token, num_digits), rem_chars))
  | _ -> None
;;

let rec resolve_keywords repl_state =
  let keywords = Repl.keywords repl_state in
  function
  | [] -> []
  | tok :: rem_toks ->
    let rest = resolve_keywords repl_state rem_toks in
    (match tok with
    | { token = Value x; pos } ->
      if Char.is_alpha @@ String.get x 0 (* repl keywords must start with a letter *)
      then (
        match Set.binary_search keywords ~compare:String.compare `First_equal_to x with
        | None ->
          let split =
            List.mapi
              ~f:(fun i a -> { token = Value (String.of_char a); pos = pos + i })
              (String.to_list x)
          in
          split @ rest
        | Some _ -> tok :: rest)
      else tok :: rest
    | _ -> tok :: rest)
;;

let tokenize ?(repl_state = Repl.init) input_str =
  let rec state_machine prev_token pos = function
    | [] -> Result.Ok []
    | (' ' | '\t') :: chars -> state_machine prev_token (pos + 1) chars
    | chars ->
      (match get_next_token prev_token chars with
      | None -> Result.Error pos
      | Some ((token, token_len), rem_chars) ->
        (match state_machine (Some token) (pos + token_len) rem_chars with
        | Result.Error pos -> Result.Error pos
        | Result.Ok lst -> Result.Ok ({ token; pos } :: lst)))
  in
  let open Result in
  String.to_list input_str |> state_machine None 0 >>| resolve_keywords repl_state
;;

let equal a b =
  match a, b with
  | Value x, Value y -> String.equal x y
  | LeftParenthesis, LeftParenthesis
  | RightParenthesis, RightParenthesis
  | Comma, Comma
  | Equals, Equals
  | Plus, Plus
  | Minus, Minus
  | Multiply, Multiply
  | Divide, Divide
  | Exponentiate, Exponentiate
  | Negate, Negate -> true
  | _ -> false
;;
