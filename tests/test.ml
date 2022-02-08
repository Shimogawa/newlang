let () =
  let open Alcotest in
  run "newlang_core"
    [
      ("string", String_tests.test_cases);
      ("function", Function_tests.test_cases);
      ("values", Val_tests.test_cases);
    ]
