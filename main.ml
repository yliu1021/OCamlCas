open Core_kernel
open Async
open Yojson

let rec process_input client_addr repl_state r w =
  Reader.read_line r
  >>= function
  | `Eof ->
    Stdio.printf "Client \"%s\" disconnected\n" client_addr;
    Out_channel.flush stdout;
    Writer.write w "exiting...";
    Writer.flushed w
  | `Ok input_line ->
    let new_repl_state, output = Engine.eval repl_state input_line in
    Writer.write w (Basic.pretty_to_string output);
    Writer.write w "\n";
    Writer.flushed w >>= fun () -> process_input client_addr new_repl_state r w
;;

let run port () =
  let host_and_port =
    Stdio.printf "Starting server on port: %d\n" port;
    Out_channel.flush stdout;
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun addr r w ->
        let client_addr = Socket.Address.Inet.to_string addr in
        Stdio.printf "Client \"%s\" connected\n" client_addr;
        Out_channel.flush stdout;
        process_input client_addr Repl_state.init r w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"Runs a calculator server"
    Command.Param.(map (anon ("port" %: int)) ~f:run)
  |> Command.run ~version:"1.0" ~build_info:"1"
;;
