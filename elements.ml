let fail reason = raise (Failure reason)

let escape_str s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\b' -> Buffer.add_string buf "\\b"
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

type stmt =
  | StmtEmpty
  | StmtExpr of expr
  | StmtAssign of string * expr
  | StmtReturn of expr
  | StmtFor of for_stmt_t

and expr =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Ident of string
  | Plus of expr * expr
  | Minus of expr * expr
  | UMinus of expr
  | Times of expr * expr
  | Div of expr * expr
  | Not of expr
  | Eq of expr * expr
  | Neq of expr * expr
  | LAnd of expr * expr
  | LOr of expr * expr
  | Fun of { name : string option; args : string list; body : stmt list }
  | Call of { expr : expr; params : expr list }
  | If of if_expr_t

and if_expr_t = { condition : expr; ifstmts : stmt list; elsestmts : stmt list }

and for_stmt_t = {
  init_stmt : stmt;
  cond : expr;
  postexec_stmt : stmt;
  body : stmt list;
}

type val_t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Fun of fun_t
  | Builtin of builtin_fun_t

and builtin_fun_t = {
  name : string;
  args : string list;
  isvararg : bool;
  f : (string, val_t) Hashtbl.t -> val_t list -> val_t;
}

and fun_t = {
  name : string option;
  args : string list;
  body : stmt list;
  vars : (string, val_t) Hashtbl.t;
}

let fun_to_string f =
  let rec arg_list_tos args =
    match args with
    | [] -> ""
    | [ x ] -> x
    | x :: xs -> Printf.sprintf "%s, %s" x (arg_list_tos xs)
  in
  match f with
  | Fun { name = n; args; body = _; _ } ->
      Printf.sprintf "function %s(%s) { ... }"
        (match n with None -> "" | Some x -> x)
        (arg_list_tos args)
  | Builtin f ->
      Printf.sprintf "function %s(%s%s) { [builtin] }" f.name
        (arg_list_tos f.args)
        (if f.isvararg then if List.length f.args = 0 then "..." else ", ..."
        else "")
  | _ -> fail "fun to string"

let repr_of_val v =
  match v with
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Int x -> string_of_int x
  | Float f -> string_of_float f
  | String s -> Printf.sprintf "\"%s\"" (escape_str s)
  | Fun _ | Builtin _ -> fun_to_string v

let string_of_val v = match v with String s -> s | _ -> repr_of_val v

let int_of_val v =
  match v with
  | Int i -> i
  | Bool b -> if b then 1 else 0
  | Float f -> int_of_float f
  | Null -> 0
  | String s -> (
      match int_of_string_opt s with None -> fail "wrong format" | Some i -> i)
  | Fun _ | Builtin _ -> fail "cannot cast function to int"

let bool_of_val v =
  match v with
  | Null -> false
  | Bool b -> b
  | String s -> String.length s != 0
  | Int i -> i != 0
  | Float f -> f != 0.0
  | _ -> true
