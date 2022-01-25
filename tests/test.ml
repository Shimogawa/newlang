let () =
  let open Alcotest in
  run "newlang_core"
    [ ("string", [ test_case "Test string" `Quick String_tests.test_str ]) ]
