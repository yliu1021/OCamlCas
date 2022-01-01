open Base

let tests =
  let open Expr in
  [ "3", node "3", "3"
  ; "3 + 24", node "3" + node "24", "3 + 24"
  ; "sin(3)^3 4", (("sin" #> (node "3")) ** node "3") * node "4", "sin(3)^3 * 4"
  ; ( "3 * max(5 sin(3))"
    , node "3" * ("max" #> (node "5" * ("sin" #> (node "3"))))
    , "3 * max(5 * sin(3))" )
  ; ( "3 - max(5 / sin(3))"
    , node "3" - ("max" #> (node "5" / ("sin" #> (node "3"))))
    , "3 - max(5 / sin(3))" )
  ; "max(3, 5) = 5", "max" #> (node "3" @ node "5") = node "5", "max(3, 5) = 5"
  ; ( "max(3, 5, sin(2)) = 5"
    , "max" #> (node "3" @ node "5" @ ("sin" #> (node "2"))) = node "5"
    , "max(3, 5, sin(2)) = 5" )
  ; "3 - -5", node "3" - ( -! ) (node "5"), "3 - -5"
  ; "1^2^3", node "1" ** node "2" ** node "3", "1^2^3"
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
    | (test_str, expected, expected_str) :: rem_tests ->
      (match Parser.parse test_str with
      | Result.Error pos -> Stdio.printf "Test \"%s\" failed at pos: %d\n" test_str pos
      | Result.Ok actual ->
        let succeeded =
          if Expr.equal expected actual
             && String.equal (Expr.to_string actual) expected_str
          then (
            Stdio.printf "Test \"%s\" passed\n" test_str;
            true)
          else (
            Stdio.printf "Test \"%s\" failed\n" test_str;
            Stdio.printf
              "Expected: \"%s\" \"%s\"\n"
              (Sexp.to_string (Expr.sexp_of_t expected))
              expected_str;
            Stdio.printf
              "Actual: \"%s\" \"%s\"\n"
              (Sexp.to_string (Expr.sexp_of_t actual))
              (Expr.to_string actual);
            false)
        in
        run_tests (if succeeded then num_failed else num_failed + 1) rem_tests)
  in
  Stdio.print_endline "Starting expr tests";
  Stdio.Out_channel.flush Stdio.stdout;
  run_tests 0 tests
;;
