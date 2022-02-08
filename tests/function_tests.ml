open Newlang_core

let test_simple_fun () =
  Alcotest.(check Common.val_t)
    "same val" Null
    (Common.execute {|f = function() {}; f();|};
     States.newlang_state#last_expr_val)

let test_fun_with_ret () =
  Alcotest.(check Common.val_t)
    "same val" (String "hi")
    (Common.execute {|f = function() { return "hi"; }; f();|};
     States.newlang_state#last_expr_val)

let test_fun_with_args () =
  Alcotest.(check Common.val_t)
    "same val" (String "abc")
    (Common.execute
       {|f = function(x, y, z) { return x + y + z; }; f("a", "b", "c");|};
     States.newlang_state#last_expr_val)

let test_cases =
  [
    Alcotest.test_case "simple function" `Quick test_simple_fun;
    Alcotest.test_case "function with return" `Quick test_fun_with_ret;
    Alcotest.test_case "function with args" `Quick test_fun_with_args;
  ]
