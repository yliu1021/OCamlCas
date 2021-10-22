open Base

let float_to_string x =
  let float_chars = Printf.sprintf "%.15f" x |> String.to_list in
  let rec remove_prefix_zeros = function
    | [] -> []
    | '0' :: rem -> remove_prefix_zeros rem
    | '.' :: rem -> rem
    | num -> num
  in
  let rev_parsed_float = List.rev float_chars |> remove_prefix_zeros in
  List.rev rev_parsed_float |> String.of_char_list
;;

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
        repl_state, float_to_string res))
;;
