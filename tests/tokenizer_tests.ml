open Base
open Tokenizer

let rec tokenize_results_match actual expected =
  match actual, expected with
  | [], [] -> true
  | { token = tok_a; pos = pos_a } :: rem_a, { token = tok_b; pos = pos_b } :: rem_b ->
    if Tokenizer.equal tok_a tok_b && pos_a = pos_b
    then tokenize_results_match rem_a rem_b
    else false
  | _ -> false
;;

let test_results_match actual expected =
  match actual, expected with
  | Result.Error x, Result.Error y -> x = y
  | Result.Ok x, Result.Ok y -> if tokenize_results_match x y then true else false
  | _ -> false
;;

let tests =
  [ "3", Result.Ok [ { token = Value "3"; pos = 0 } ]
  ; "3.5", Result.Ok [ { token = Value "3.5"; pos = 0 } ]
  ; ".3", Result.Ok [ { token = Value ".3"; pos = 0 } ]
  ; "3.5.6", Result.Error 0
  ; ( "(1)-2"
    , Result.Ok
        [ { token = LeftParenthesis; pos = 0 }
        ; { token = Value "1"; pos = 1 }
        ; { token = RightParenthesis; pos = 2 }
        ; { token = Minus; pos = 3 }
        ; { token = Value "2"; pos = 4 }
        ] )
  ; ( "(35)"
    , Result.Ok
        [ { token = LeftParenthesis; pos = 0 }
        ; { token = Value "35"; pos = 1 }
        ; { token = RightParenthesis; pos = 3 }
        ] )
  ; ( "a + .35"
    , Result.Ok
        [ { token = Value "a"; pos = 0 }
        ; { token = Plus; pos = 2 }
        ; { token = Value ".35"; pos = 4 }
        ] )
  ; ( "a - -.35"
    , Result.Ok
        [ { token = Value "a"; pos = 0 }
        ; { token = Minus; pos = 2 }
        ; { token = Negate; pos = 4 }
        ; { token = Value ".35"; pos = 5 }
        ] )
  ; ( "3**5"
    , Result.Ok
        [ { token = Value "3"; pos = 0 }
        ; { token = Exponentiate; pos = 1 }
        ; { token = Value "5"; pos = 3 }
        ] )
  ; ( "a / -b"
    , Result.Ok
        [ { token = Value "a"; pos = 0 }
        ; { token = Divide; pos = 2 }
        ; { token = Negate; pos = 4 }
        ; { token = Value "b"; pos = 5 }
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
