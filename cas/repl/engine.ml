open Base

let eval repl_state str =
  match Tokenizer.tokenize ~repl_state str with
  | Result.Error pos ->
    repl_state, String.concat [ "error: tokenize failed at pos "; Int.to_string pos ]
  | Result.Ok tokens ->
    (match Parser.parse tokens with
    | None -> repl_state, "error: failed to parse tokens"
    | Some parse_tree ->
      (match Expr.of_parse_tree parse_tree with
      | Result.Error pos ->
        ( repl_state
        , String.concat [ "error: failed to convert token at pos "; Int.to_string pos ] )
      | Result.Ok expr_tree ->
        let res = Eval.to_float expr_tree in
        repl_state, Float.to_string res))
;;
