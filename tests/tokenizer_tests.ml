open Base
open Tokenizer

let rec tokenize_results_match actual expected =
  match actual, expected with
  | [], [] -> true
  | a :: rem_a, b :: rem_b ->
    if Tokenizer.equal a b then tokenize_results_match rem_a rem_b else false
  | _ -> false
;;

let test_results_match actual expected =
  match actual, expected with
  | Result.Error x, Result.Error y -> x = y
  | Result.Ok x, Result.Ok y -> if tokenize_results_match x y then true else false
  | _ -> false
;;

let tests =
  [ "3", Result.Ok [ { token = Constant; pos = 0; value = "3" } ]
  ; "3.5", Result.Ok [ { token = Constant; pos = 0; value = "3.5" } ]
  ; ".3", Result.Ok [ { token = Constant; pos = 0; value = ".3" } ]
  ; "pi", Result.Ok [ { token = Constant; pos = 0; value = "pi" } ]
  ; "i", Result.Ok [ { token = Constant; pos = 0; value = "i" } ]
  ; "e", Result.Ok [ { token = Constant; pos = 0; value = "e" } ]
  ; "3.5.6", Result.Error 0
  ; ( "(1)-2"
    , Result.Ok
        [ { token = LeftParenthesis; pos = 0; value = "(" }
        ; { token = Constant; pos = 1; value = "1" }
        ; { token = RightParenthesis; pos = 2; value = ")" }
        ; { token = Minus; pos = 3; value = "-" }
        ; { token = Constant; pos = 4; value = "2" }
        ] )
  ; ( "(35)"
    , Result.Ok
        [ { token = LeftParenthesis; pos = 0; value = "(" }
        ; { token = Constant; pos = 1; value = "35" }
        ; { token = RightParenthesis; pos = 3; value = ")" }
        ] )
  ; ( "a + .35"
    , Result.Ok
        [ { token = Variable; pos = 0; value = "a" }
        ; { token = Plus; pos = 2; value = "+" }
        ; { token = Constant; pos = 4; value = ".35" }
        ] )
  ; ( "a - -.35"
    , Result.Ok
        [ { token = Variable; pos = 0; value = "a" }
        ; { token = Minus; pos = 2; value = "-" }
        ; { token = Negate; pos = 4; value = "-" }
        ; { token = Constant; pos = 5; value = ".35" }
        ] )
  ; ( "3**5"
    , Result.Ok
        [ { token = Constant; pos = 0; value = "3" }
        ; { token = Exponentiate; pos = 1; value = "**" }
        ; { token = Constant; pos = 3; value = "5" }
        ] )
  ; ( "3^5"
    , Result.Ok
        [ { token = Constant; pos = 0; value = "3" }
        ; { token = Exponentiate; pos = 1; value = "^" }
        ; { token = Constant; pos = 2; value = "5" }
        ] )
  ; ( "ap / -sin"
    , Result.Ok
        [ { token = Variable; pos = 0; value = "a" }
        ; { token = Variable; pos = 1; value = "p" }
        ; { token = Divide; pos = 3; value = "/" }
        ; { token = Negate; pos = 5; value = "-" }
        ; { token = Function; pos = 6; value = "sin" }
        ] )
  ]
;;

let () =
  let sexp_of_test_result =
    Result.sexp_of_t (List.sexp_of_t Tokenizer.sexp_of_t) Int.sexp_of_t
  in
  let rec run_tests num_failed = function
    | [] ->
      Stdio.printf "Tests finished";
      if num_failed > 0
      then (
        Stdio.printf " - %d failed\n" num_failed;
        Caml.exit 1)
      else Stdio.print_endline " - all passed"
    | (test_str, expected) :: rem_tests ->
      let actual = tokenize test_str in
      let succeeded = test_results_match actual expected in
      if succeeded
      then Stdio.printf "Test: \"%s\" passed\n" test_str
      else (
        Stdio.printf "Test: \"%s\" failed - " test_str;
        Stdio.printf "Expected: %s " (Sexp.to_string (sexp_of_test_result expected));
        Stdio.printf "Actual: %s\n" (Sexp.to_string (sexp_of_test_result actual)));
      run_tests (if succeeded then num_failed else num_failed + 1) rem_tests
  in
  run_tests 0 tests
;;
