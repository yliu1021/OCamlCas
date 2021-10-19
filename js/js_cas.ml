open Js_of_ocaml

let () =
  Js.export_all
    (object%js
       method eval x =
         let _, out = Engine.eval Repl_state.init (Js.to_string x) in
         Js.string out
    end)
;;
