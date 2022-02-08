open Newlang_core

let test_str () =
  Alcotest.(check Common.val_t)
    "same string" (String "abc")
    (Common.execute {|"abc";|};
     States.newlang_state#last_expr_val)

let test_cases = [ Alcotest.test_case "simple string" `Quick test_str ]
