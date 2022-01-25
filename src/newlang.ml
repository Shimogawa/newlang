open Newlang_core

let _ =
  try
    while true do
      print_string "> ";
      flush stdout;
      let lexbuf = Lexing.from_channel stdin in
      try
        let result = Parser.main Lexer.token lexbuf in
        Core.execute result;
        match States.newlang_state#last_expr_val with
        | Null -> ()
        | x -> print_endline (Elements.repr_of_val x)
      with e -> print_endline (Printexc.to_string e)
    done
  with Lexer.Eof -> exit 0
