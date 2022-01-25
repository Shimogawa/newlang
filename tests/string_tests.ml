open Newlang_core

let test_str () =
  Alcotest.(check string)
    "same string" "abc"
    (Core.execute
       (Parser.main Lexer.token (Lexing.from_string {|
    "abc";
    |}));
     Elements.string_of_val States.newlang_state#last_expr_val)
