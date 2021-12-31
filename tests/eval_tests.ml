open Base
open Expr
open Eval

let tests =
  [ node "3", 3.
  ; node "3" +| node "24", 27.
  ; (("sin" @@| node "3") **| node "3") *| node "4", 0.01124153894
  ; node "3" *| ("max" @@| (node "5" *| ("sin" @@| node "3"))), 2.1168001209
  ; node "3" -| ("max" @@| (node "5" /| ("sin" @@| node "3"))), -32.4308369787
  ; "max" @@| node "3" @| node "5", 5.
  ; node "3" -| ( -/ ) (node "5"), 8.
  ; "exp" @@| node "1", 2.718281828459045
  ; "ln" @@| node "e", 1.
  ; "sin" @@| node "pi", 0.
  ; "cos" @@| node "pi", -1.
  ; node "x", Float.nan
  ; "sqrt" @@| node "2", 1.4142135624
  ; "cbrt" @@| node "27", 3.
  ; "log" @@| node "10", 1.
  ; "min" @@| (node "14" @| node "5" @| node "1"), 1.
  ; "abs" @@| node "-1", 1.
  ; "sgn" @@| node "325", 1.
  ; "tan" @@| node "1", 1.5574077246549023
  ; "sec" @@| node "1", 1.8508157176809255
  ; "csc" @@| node "1", 1.1883951057781212
  ; "cot" @@| node "1", 0.6420926159343306
  ; "asin" @@| node "1", 1.5707963267948966
  ; "acos" @@| node "0.5", 1.0471975511965979
  ; "atan" @@| node "1", 0.7853981633974483
  ; "asec" @@| node "1.5", 0.8410686705679303
  ; "acsc" @@| node "1", 1.5707963267948966
  ; "acot" @@| node "1", 0.7853981633974483
  ]
;;

let () =
  let rec run_tests num_failed = function
    | [] ->
      Stdio.printf "Tests finished";
      if num_failed > 0
      then (
        Stdio.printf " - %d failed\n" num_failed;
        Caml.exit 1)
      else Stdio.print_endline " - all passed"
    | (test_expr, expected) :: rem_tests ->
      let actual = to_float test_expr in
      let test_expr_str = Sexp.to_string (sexp_of_t test_expr) in
      let succeeded =
        if (Float.is_nan expected && Float.is_nan actual) || Float.(abs (actual -. expected) < 1e-6)
        then (
          Stdio.printf "Test \"%s\" passed\n" test_expr_str;
          true)
        else (
          Stdio.printf "Test \"%s\" failed\n" test_expr_str;
          Stdio.printf "Expected: %f\n" expected;
          Stdio.printf "Actual:   %f\n" actual;
          false)
      in
      run_tests (if succeeded then num_failed else num_failed + 1) rem_tests
  in
  run_tests 0 tests
;;
