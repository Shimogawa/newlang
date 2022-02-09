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

let test_fun_closure () =
  Alcotest.(check Common.val_t)
    "closure 1" (String "a")
    (Common.execute
       {|f = function() { return function() { return "a"; }; }; f()();|};
     States.newlang_state#last_expr_val);
  Alcotest.(check Common.val_t)
    "closure 2" (Int 2)
    (Common.execute
       {|
       f = function() { a = 0; return function() { a = a + 1; return a; }; };
       c = f();
       c();
       c();
       |};
     States.newlang_state#last_expr_val);
  Alcotest.(check Common.val_t)
    "closure 3" (Int 4)
    (Common.execute
       {|
        a = 2;
        f = function() { return function() { a = a + 1; return a; }; };
        c = f();
        c();
        c();
        |};
     States.newlang_state#last_expr_val);
  Alcotest.(check Common.val_t)
    "closure 4" (Int 6)
    (Common.execute
       {|
        f = function() {
          a = 2;
          return function() {
            b = 0;
            return function () {
              a = a + 1;
              b = b + 1;
              return a + b;
            };
          };
        };
        c = f()();
        c();
        c();
        |};
     States.newlang_state#last_expr_val);
  Alcotest.(check Common.val_t)
    "closure 5" (Int 5)
    (Common.execute
       {|
        delay = function(f) {
          result = null;
          return function() {
            if (result == null) {
              result = f();
            };
            return result;
          };
        };
        d = delay(function () { return 5; });
        d();
        |};
     States.newlang_state#last_expr_val)

let test_cases =
  [
    Alcotest.test_case "simple function" `Quick test_simple_fun;
    Alcotest.test_case "function with return" `Quick test_fun_with_ret;
    Alcotest.test_case "function with args" `Quick test_fun_with_args;
    Alcotest.test_case "closure" `Quick test_fun_closure;
  ]
