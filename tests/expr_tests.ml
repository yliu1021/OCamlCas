open Base
open Expr

let get_expr str =
  match Tokenizer.tokenize str with
  | Result.Error pos -> Result.Error pos
  | Result.Ok tokens ->
    (match Parser.parse tokens with
    | None -> Result.Error (-1)
    | Some parse_tree -> of_parse_tree parse_tree)
;;

let tests =
  [ "3", node "3"
  ; "3 + 24", node "3" +| node "24"
  ; "sin(3)^3 4", (("sin" @@| node "3") **| node "3") *| node "4"
  ; "3 * max(5 sin(3))", (node "3") *| ("max"@@|((node "5") *| ("sin"@@|(node "3"))))
  ]
;;

let () =
  (* let () =
    match Tokenizer.tokenize "2sin(1)sin(5)" with
    | Result.Error _ -> Caml.exit 0
    | Result.Ok tokens ->
      (match parse tokens with
      | None -> Caml.exit 0
      | Some t ->
        Stdio.print_endline (Sexp.to_string (Parser.sexp_of_t t));
        Caml.exit 0)
  in *)
  let rec run_tests num_failed = function
    | [] ->
      Stdio.printf "Tests finished\n";
      if num_failed > 0
      then (
        Stdio.printf " - %d failed\n" num_failed;
        Caml.exit 1)
      else Stdio.print_endline " - all passed"
    | (test_str, expected) :: rem_tests ->
      (match get_expr test_str with
      | Result.Error pos -> Stdio.printf "Test \"%s\" failed at pos: %d\n" test_str pos
      | Result.Ok actual ->
        let succeeded =
          if equal expected actual
          then (
            Stdio.printf "Test \"%s\" passed\n" test_str;
            true)
          else (
            Stdio.printf "Test \"%s\" failed\n" test_str;
            Stdio.printf "Expected: \"%s\"\n" (Sexp.to_string (sexp_of_t expected));
            Stdio.printf "Actual: \"%s\"\n" (Sexp.to_string (sexp_of_t actual));
            false)
        in
        run_tests (if succeeded then num_failed else num_failed + 1) rem_tests)
  in
  Stdio.print_endline "Starting expr tests";
  Stdio.Out_channel.flush Stdio.stdout;
  run_tests 0 tests
;;
