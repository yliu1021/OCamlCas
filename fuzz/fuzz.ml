open Base
open Stdio

let parse_expr str =
  match Tokenizer.tokenize str with
  | Result.Error _ -> None
  | Result.Ok tokens ->
    (match Parser.parse tokens with
    | None -> None
    | Some parse_tree ->
      (match Expr.of_parse_tree parse_tree with
      | Result.Error _ -> None
      | Result.Ok expr -> Some expr))
;;

let read_file input_str =
  match parse_expr input_str with
  | None -> ()
  | Some expr ->
    let expr_str = Expr.to_string expr in
    (match parse_expr expr_str with
    | None -> assert false
    | Some parsed_expr -> assert (Expr.equal expr parsed_expr); printf "%s\n" expr_str)
;;

let () =
  match Sys.get_argv () with
  | [| _; filename |] -> In_channel.read_all filename |> read_file
  | _ -> assert false
;;
