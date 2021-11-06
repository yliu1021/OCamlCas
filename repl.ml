open Stdio
open Engine
open Yojson

let () =
  let rec run_loop repl_state =
    print_string "> ";
    Out_channel.flush stdout;
    match In_channel.input_line stdin with
    | None -> print_endline "exiting..."
    | Some user_input ->
      let new_repl_state, repl_output = eval repl_state user_input in
      print_endline (Basic.pretty_to_string repl_output);
      run_loop new_repl_state
  in
  run_loop Repl_state.init
;;
