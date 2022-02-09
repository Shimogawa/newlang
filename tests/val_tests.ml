open Newlang_core

let test_int () =
  Alcotest.(check Common.val_t)
    "same val" (Int 3)
    (Common.execute {|3;|};
     States.newlang_state#last_expr_val)

let test_float () =
  Alcotest.(check Common.val_t)
    "same val" (Float 7.0)
    (Common.execute {|7.0;|};
     States.newlang_state#last_expr_val)

let test_add () =
  Alcotest.(check Common.val_t)
    "int add" (Int 7)
    (Common.execute {|3 + 4;|};
     States.newlang_state#last_expr_val);
  Alcotest.(check Common.val_t)
    "string add 1" (String "34")
    (Common.execute {|"3" + 4;|};
     States.newlang_state#last_expr_val);
  Alcotest.(check Common.val_t)
    "string add 2" (String "34")
    (Common.execute {|"3" + "4";|};
     States.newlang_state#last_expr_val);
  Alcotest.(check Common.val_t)
    "string add 3" (String "34")
    (Common.execute {|3 + "4";|};
     States.newlang_state#last_expr_val);
  Alcotest.(check Common.val_t)
    "float add 1" (Float 7.0)
    (Common.execute {|3 + 4.0;|};
     States.newlang_state#last_expr_val);
  Alcotest.(check Common.val_t)
    "float add 2" (Float 7.0)
    (Common.execute {|3.0 + 4;|};
     States.newlang_state#last_expr_val)

let test_cases =
  [
    Alcotest.test_case "integer" `Quick test_int;
    Alcotest.test_case "float" `Quick test_float;
    Alcotest.test_case "add" `Quick test_add;
  ]
