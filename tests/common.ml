open Newlang_core

let execute str =
  try
    let lexbuf = Lexing.from_string str in
    while true do
      Core.execute (Parser.main Lexer.token lexbuf)
    done
  with Lexer.Eof -> ()

let val_t =
  let pp_val_t ppf (x : Elements.val_t) =
    match x with
    | String s -> Fmt.pf ppf "%s" s
    | Int i -> Fmt.pf ppf "%d" i
    | Float f -> Fmt.pf ppf "%.7f" f
    | Null -> Fmt.pf ppf "val_t.null"
    | _ -> ()
  in
  Alcotest.testable pp_val_t ( = )
