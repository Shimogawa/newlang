open Elements

let _exit (_ : (string, val_t) Hashtbl.t) (varargs : val_t list) : val_t =
  let exit_val =
    match List.length varargs with
    | 1 -> (
        match List.nth varargs 0 with
        | Int i -> i
        | _ -> fail "argument should be integer")
    | 0 -> 0
    | _ -> fail "exit only take 0 or 1 argument"
  in
  ignore (exit exit_val);
  Null

let _sprintf _ varargs =
  match varargs with
  | [] -> fail "no format given"
  | fmt :: args ->
      let fmt =
        String.to_seq
          (match fmt with String s -> s | _ -> fail "fmt must be string")
      and buf = Buffer.create 64 in
      let rec _pr buf (fmt : char Seq.t) (args : val_t list) =
        match args with
        | [] -> String.of_seq fmt |> Buffer.add_string buf
        | a :: aas -> (
            match fmt () with
            | Nil -> ()
            | Cons (f, fs) -> (
                match f with
                | '{' -> (
                    match fs () with
                    | Nil -> Buffer.add_char buf '%'
                    | Cons (f, fs) -> (
                        match f with
                        | '{' -> Buffer.add_char buf '{'
                        | '}' ->
                            string_of_val a |> Buffer.add_string buf;
                            _pr buf fs aas
                        | _ ->
                            Buffer.add_char buf f;
                            _pr buf fs args))
                | _ ->
                    Buffer.add_char buf f;
                    _pr buf fs args))
      in
      _pr buf fmt args;
      String (Buffer.contents buf)

let _printf _ varargs =
  match _sprintf [] varargs with
  | String s ->
      print_string s;
      Null
  | _ -> Null

let _print _ varargs =
  let rec _pr varargs =
    match varargs with
    | [] -> print_newline ()
    | [ x ] -> string_of_val x |> print_endline
    | x :: xs ->
        string_of_val x |> print_string;
        print_char ' ';
        _pr xs
  in
  _pr varargs;
  Null

let _string args _ =
  match Hashtbl.find_opt args "x" with
  | None -> fail "string requires exactly 1 argument"
  | Some v -> String (string_of_val v)

let _int args _ =
  match Hashtbl.find_opt args "x" with
  | None -> fail "int requires exactly 1 argument"
  | Some v -> Int (int_of_val v)

let _char args _ =
  match Hashtbl.find_opt args "x" with
  | None -> fail "char requires exactly 1 argument"
  | Some v ->
      let buf = Buffer.create 1 in
      Uchar.of_int (int_of_val v) |> Buffer.add_utf_8_uchar buf;
      String (Buffer.contents buf)

let _ord args _ =
  match Hashtbl.find_opt args "x" with
  | None -> fail "ord requires exactly 1 argument"
  | Some v -> (
      match v with
      | String s -> (
          let de = Uutf.decoder ~encoding:`UTF_8 (`String s) in
          match Uutf.decode de with
          | `Uchar c -> Int (Uchar.to_int c)
          | _ -> fail "malformatted string")
      | _ -> fail "ord takes in string")

let _get_ret_val _ _ = States.newlang_state#last_ret_val

let _length args _ =
  match Hashtbl.find_opt args "x" with
  | None -> fail "length requires exactly 1 argument"
  | Some v -> (
      match v with
      | String s -> Int (Utils.utf8_string_len s)
      | _ -> fail "no length")

let builtins =
  [
    ("exit", Builtin { name = "exit"; args = []; isvararg = true; f = _exit });
    ( "sprintf",
      Builtin { name = "sprintf"; args = []; isvararg = true; f = _sprintf } );
    ( "printf",
      Builtin { name = "printf"; args = []; isvararg = true; f = _printf } );
    ("print", Builtin { name = "print"; args = []; isvararg = true; f = _print });
    ( "string",
      Builtin { name = "string"; args = [ "x" ]; isvararg = false; f = _string }
    );
    ("int", Builtin { name = "int"; args = [ "x" ]; isvararg = false; f = _int });
    ( "char",
      Builtin { name = "char"; args = [ "x" ]; isvararg = false; f = _char } );
    ("ord", Builtin { name = "ord"; args = [ "x" ]; isvararg = false; f = _ord });
    ( "$_",
      Builtin { name = "$_"; args = []; isvararg = false; f = _get_ret_val } );
    ( "length",
      Builtin { name = "length"; args = [ "x" ]; isvararg = false; f = _length }
    );
  ]
  |> List.to_seq |> Hashtbl.of_seq
