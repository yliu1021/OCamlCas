open Base

let error msg pos = `Assoc [ "message", `String msg; "position", `Int pos ]

let eval repl_state str =
  let eval_raw repl_state str =
    match Parser.parse ~repl_state str with
    | Result.Error pos ->
      ( repl_state
      , `Assoc [ "success", `Bool false; "error", error "failed to tokenize" pos ] )
    | Result.Ok expr ->
      let res = Eval.to_float expr in
      let number = if Float.is_nan res then `Null else `Float res in
      repl_state, `Assoc [ "success", `Bool true; "number", number ]
  in
  let repl_state, raw_response = eval_raw repl_state str in
  repl_state, `Assoc [ "query", `String str; "response", raw_response ]
;;
