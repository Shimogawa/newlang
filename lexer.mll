{
open Parser
exception Eof

let strbuf = Buffer.create 256
let char_for_backslash c =
  match c with
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\b'
  | 't' -> '\t'
  | c   -> c
}

let idtf_char = ['a'-'z' 'A'-'Z' '$' '_'] | [^'\x00'-'\x7f']
let identifier = idtf_char idtf_char*
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let integer = digit+ | "0x" hexdigit+
let float = digit+ ('.' digit+)?
let white = [' ' '\t' '\n' '\r']

let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r']

rule token = parse
      white { token lexbuf }
    | integer as i { INT(int_of_string i) }
    | float as f { FLOAT(float_of_string f) }
    | '=' {ASSIGN}
    | '+' {OP_PLUS}
    | '-' {OP_MINUS}
    | '*' {OP_TIMES}
    | '/' {OP_DIV}
    | "==" {OP_EQ}
    | "!=" {OP_NEQ}
    | '!' {OP_LNOT}
    | "&&" {OP_LAND}
    | "||" {OP_LOR}
    | '(' {LPAREN}
    | ')' {RPAREN}
    | '{' {LCURLY}
    | '}' {RCURLY}
    | ';' {SEMICOL}
    | ',' {COMMA}
    | '"' { Buffer.clear strbuf;
            string lexbuf;
            STRING (Buffer.contents strbuf) }
    | "else" {KWD_ELSE}
    | "false" {BOOL(false)}
    | "for" {KWD_FOR}
    | "function" {KWD_FUNCTION}
    | "if" {KWD_IF}
    | "null" {KWD_NULL}
    | "return" {KWD_RETURN}
    | "true" {BOOL(true)}
    | identifier as idtf { IDENT(idtf) }
    | eof { raise Eof }
    | _ { failwith "lexer error" }
and string = parse
  | '"' { () }
  | '\\' (backslash_escapes as c)
        { Buffer.add_char strbuf (char_for_backslash c);
          string lexbuf }
  | _ as c { Buffer.add_char strbuf c;
             string lexbuf }
