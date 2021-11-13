open Base

let error msg pos = `Assoc [ "message", `String msg; "position", `Int pos ]

let eval repl_state str =
  let eval_raw repl_state str =
    match Tokenizer.tokenize ~repl_state str with
    | Result.Error pos ->
      (* tokenization failed *)
      ( repl_state
      , `Assoc [ "success", `Bool false; "error", error "failed to tokenize" pos ] )
    | Result.Ok tokens ->
      (match Parser.parse tokens with
      | None ->
        (* parser failed *)
        ( repl_state
        , `Assoc [ "success", `Bool false; "error", error "failed to parse tokens" (-1) ]
        )
      | Some parse_tree ->
        (match Expr.of_parse_tree parse_tree with
        | Result.Error pos ->
          (* expr failed *)
          ( repl_state
          , `Assoc
              [ "success", `Bool false
              ; "error", error "failed to convert AST to expression" pos
              ] )
        | Result.Ok expr_tree ->
          (* success *)
          let res = Eval.to_float expr_tree in
          let number = if Float.is_nan res then `Null else `Float res in
          repl_state, `Assoc [ "success", `Bool true; "number", number ]))
  in
  let repl_state, raw_response = eval_raw repl_state str in
  repl_state, `Assoc [ "query", `String str; "response", raw_response ]
;;
