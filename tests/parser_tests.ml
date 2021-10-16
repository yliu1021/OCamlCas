open Base
open Parser

let number pos value =
  Leaf Tokenizer.{ token = Constant; pos; value = Int.to_string value }
;;

let add pos left right =
  InfixOp { token = Tokenizer.{ token = Plus; pos; value = "+" }; left; right }
;;

let mult pos left right =
  InfixOp { token = Tokenizer.{ token = Multiply; pos; value = "*" }; left; right }
;;

let exp pos left right =
  InfixOp { token = Tokenizer.{ token = Exponentiate; pos; value = "^" }; left; right }
;;

let impl_mult left right =
  InfixOp { token = Tokenizer.{ token = Multiply; pos = -1; value = "" }; left; right }
;;

let sqrt pos child =
  PrefixOp { token = Tokenizer.{ token = Function; pos; value = "sqrt" }; child }
;;

let tests =
  [ "3", number 0 3
  ; "3+4", add 1 (number 0 3) (number 2 4)
  ; "3*4*5", mult 3 (mult 1 (number 0 3) (number 2 4)) (number 4 5)
  ; "sqrt(1+2)", sqrt 0 (add 6 (number 5 1) (number 7 2))
  ; "sqrt(1+2) 5", impl_mult (sqrt 0 (add 6 (number 5 1) (number 7 2))) (number 10 5)
  ; "sqrt(1+2)*5", mult 9 (sqrt 0 (add 6 (number 5 1) (number 7 2))) (number 10 5)
  ; "sqrt(1+2)^5", exp 9 (sqrt 0 (add 6 (number 5 1) (number 7 2))) (number 10 5)
  ; "5sqrt(1+2)", impl_mult (number 0 5) (sqrt 1 (add 7 (number 6 1) (number 8 2)))
  ; "5^sqrt(1+2)", exp 1 (number 0 5) (sqrt 2 (add 8 (number 7 1) (number 9 2)))
  ; ( "sqrt(3)^sqrt(1+2)"
    , exp 7 (sqrt 0 (number 5 3)) (sqrt 8 (add 14 (number 13 1) (number 15 2))) )
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
      (match Tokenizer.tokenize test_str with
      | Result.Error pos ->
        Stdio.printf "Test: \"%s\" failed to tokenize at %d\n" test_str pos;
        Caml.exit 1
      | Result.Ok tokens ->
        let succeeded =
          match parse tokens with
          | None ->
            Stdio.printf "Test: \"%s\" failed to parse\n" test_str;
            false
          | Some actual ->
            if Parser.equal actual expected
            then (
              Stdio.printf "Test: \"%s\" passed\n" test_str;
              true)
            else (
              Stdio.printf "Test: \"%s\" failed\n" test_str;
              Stdio.printf "Expected: %s\n" (Sexp.to_string (Parser.sexp_of_t expected));
              Stdio.printf "Actual:   %s\n" (Sexp.to_string (Parser.sexp_of_t actual));
              false)
        in
        run_tests (if succeeded then num_failed else num_failed + 1) rem_tests)
  in
  Stdio.print_endline "Starting parser tests";
  Stdio.Out_channel.flush Stdio.stdout;
  run_tests 0 tests
;;
