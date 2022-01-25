open Elements
open Utils

let _symtbl : (string, val_t) Hashtbl.t = Hashtbl.create 8
let _funstack : fun_t Stack.t = Stack.create ()
let is_in_fun () = not (Stack.is_empty _funstack)
let push_fun (f : fun_t) = Stack.push f _funstack
let pop_fun () = Stack.pop _funstack
let current_fun () = Stack.top _funstack

let assign_in_fun k v =
  match Stack.top_opt _funstack with
  | Some s -> Hashtbl.replace s.vars k v
  | None -> fail "not inside function"

let assign (idtf : string) (value : val_t) = Hashtbl.replace _symtbl idtf value

let go_through_funstack idtf =
  let rec helper idtf seq =
    match seq () with
    | Seq.Nil -> None
    | Seq.Cons (x, xs) -> (
        match Hashtbl.find_opt x.vars idtf with
        | None -> helper idtf xs
        | Some v -> Some v)
  in
  helper idtf (Stack.to_seq _funstack)

let get_fun_local_var idtf =
  if not (is_in_fun ()) then None
  else Hashtbl.find_opt (current_fun ()).vars idtf

let get (idtf : string) : val_t =
  match get_fun_local_var idtf with
  | None -> (
      match Hashtbl.find_opt _symtbl idtf with
      | Some x -> x
      | None -> Hashtbl.find Builtins.builtins idtf)
  | Some v -> v

let compute_captured_var (f : fun_t) =
  let defined = ref (S.of_list f.args) in
  let rec go_through_stmts stmts =
    match stmts with
    | [] -> ()
    | [ s ] -> check_stmt s
    | s :: ss ->
        check_stmt s;
        go_through_stmts ss
  and check_stmt stmt =
    match stmt with
    | StmtEmpty -> ()
    | StmtAssign (i, e) ->
        check_expr e;
        defined := S.add i !defined
    | StmtExpr e -> check_expr e
    | StmtReturn e -> check_expr e
    | StmtFor sf ->
        check_stmt sf.init_stmt;
        check_expr sf.cond;
        go_through_stmts sf.body;
        check_stmt sf.postexec_stmt
  and check_expr e =
    match e with
    | Ident name -> (
        match S.find_opt name !defined with
        | Some _ -> ()
        | None ->
            (match go_through_funstack name with
            | Some v -> Hashtbl.replace f.vars name v
            | None -> ());
            defined := S.add name !defined)
    | Plus (x, y)
    | Minus (x, y)
    | Times (x, y)
    | Div (x, y)
    | LAnd (x, y)
    | LOr (x, y)
    | Eq (x, y)
    | Neq (x, y) ->
        check_expr x;
        check_expr y
    | UMinus x | Not x -> check_expr x
    | Call { expr; _ } -> check_expr expr
    | If i ->
        check_expr i.condition;
        go_through_stmts i.ifstmts;
        go_through_stmts i.elsestmts
    | _ -> ()
  in
  go_through_stmts f.body

let as_fun_t (f : expr) =
  match f with
  | Fun x ->
      let r =
        { name = x.name; args = x.args; body = x.body; vars = Hashtbl.create 8 }
      in
      compute_captured_var r;
      Fun r
  | _ -> fail "not a function"

let rec eval (expr : expr) : val_t =
  let v =
    match expr with
    | Null -> Null
    | Bool b -> Bool b
    | Int i -> Int i
    | Float f -> Float f
    | String s -> String s
    | Ident x -> get x
    | Plus (x, y) -> (
        match (eval x, eval y) with
        | Int a, Int b -> Int (a + b)
        | Float a, Float b -> Float (a +. b)
        | Float a, Int b -> Float (a +. float_of_int b)
        | Int a, Float b -> Float (float_of_int a +. b)
        | String a, String b -> String (a ^ b)
        | x, String b -> String (string_of_val x ^ b)
        | String a, x -> String (a ^ string_of_val x)
        | _ -> fail "+")
    | Minus (x, y) -> (
        match (eval x, eval y) with
        | Int a, Int b -> Int (a - b)
        | Float a, Float b -> Float (a -. b)
        | Float a, Int b -> Float (a -. float_of_int b)
        | Int a, Float b -> Float (float_of_int a -. b)
        | _ -> fail "-")
    | Times (x, y) -> (
        match (eval x, eval y) with
        | Int a, Int b -> Int (a * b)
        | Float a, Float b -> Float (a *. b)
        | Float a, Int b -> Float (a *. float_of_int b)
        | Int a, Float b -> Float (float_of_int a *. b)
        | _ -> fail "*")
    | Div (x, y) -> (
        match (eval x, eval y) with
        | Int a, Int b -> Int (a / b)
        | Float a, Float b -> Float (a /. b)
        | Float a, Int b -> Float (a /. float_of_int b)
        | Int a, Float b -> Float (float_of_int a /. b)
        | _ -> fail "/")
    | UMinus x -> (
        match eval x with
        | Int a -> Int (-a)
        | Float a -> Float (-.a)
        | _ -> fail "-")
    | Not x -> Bool (not (bool_of_val (eval x)))
    | Eq (x, y) -> Bool (eval x = eval y)
    | Neq (x, y) -> Bool (eval x <> eval y)
    | LAnd (x, y) ->
        if bool_of_val (eval x) then Bool (bool_of_val (eval y)) else Bool false
    | LOr (x, y) ->
        if bool_of_val (eval x) then Bool true else Bool (bool_of_val (eval y))
    | Fun _ -> as_fun_t expr
    | Call { expr; params } -> execute_call (eval expr) params
    | If ie ->
        if eval ie.condition |> bool_of_val then execute_stmts ie.ifstmts
        else execute_stmts ie.elsestmts;
        States.newlang_state#last_expr_val
  in
  States.newlang_state#set_last_expr_val v;
  v

and eval_multiple exprs =
  match exprs with
  | [] -> []
  | [ e ] -> [ eval e ]
  | e :: es -> eval e :: eval_multiple es

and execute_call (v : val_t) params =
  match v with
  | Fun f -> (
      if List.length params != List.length f.args then
        fail "inconsistent number of arguments"
      else push_fun f;
      assign_in_fun_mul f.args params;
      execute_stmts f.body;
      pop_fun () |> ignore;
      let r = States.newlang_state#last_stmt_ret_val in
      States.newlang_state#set_last_stmt_ret_val None;
      match r with
      | Some v ->
          States.newlang_state#set_last_ret_val v;
          v
      | None -> fail "internal error: function must have return value")
  | Builtin f ->
      let h, t = split_list (List.length f.args) (eval_multiple params) in
      f.f (zip_to_hashtbl f.args h) t
  | _ -> fail "call"

and execute stmt =
  match stmt with
  | StmtEmpty -> ()
  | StmtAssign (name, v) ->
      let c = eval v in
      if is_in_fun () then assign_in_fun name c else assign name c
  | StmtExpr e -> ignore (eval e)
  | StmtReturn e ->
      if is_in_fun () then
        let r = eval e in
        States.newlang_state#set_last_stmt_ret_val (Some r)
      else fail "not in function"
  | StmtFor sf ->
      States.newlang_state#set_loop_end_sig false;
      execute sf.init_stmt;
      while not States.newlang_state#loop_end_sig do
        if bool_of_val (eval sf.cond) then (
          execute_stmts sf.body;
          execute sf.postexec_stmt)
        else States.newlang_state#set_loop_end_sig true
      done;
      States.newlang_state#set_last_expr_val Null

and execute_stmts stmts : unit =
  match stmts with
  | [] -> (
      match States.newlang_state#last_stmt_ret_val with
      | None -> States.newlang_state#set_last_stmt_ret_val (Some Null)
      | Some _ -> ())
  | [ s ] ->
      execute s;
      execute_stmts []
  | s :: ss -> (
      execute s;
      match States.newlang_state#last_stmt_ret_val with
      | None -> execute_stmts ss
      | Some _ -> ())

and assign_in_fun_mul idtfs exprs =
  match (idtfs, exprs) with
  | [], [] -> ()
  | [ k ], [ e ] -> assign_in_fun k (eval e)
  | k :: ks, e :: es ->
      assign_in_fun k (eval e);
      assign_in_fun_mul ks es
  | _ -> fail "wrong number of arguments"
