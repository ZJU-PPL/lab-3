{
open Token
open Lexing

exception Error of string * position

let kws = Hashtbl.of_seq @@ List.to_seq
    [ ("let"        , TK_LET)
    ; ("in"         , TK_IN)
    ; ("if"         , TK_IF)
    ; ("then"       , TK_THEN)
    ; ("else"       , TK_ELSE)
    ; ("end"        , TK_END)
    ]

let ssyms = Hashtbl.of_seq @@ List.to_seq
    [ ('.'  , TK_DOT)
    ; (':'  , TK_COLON)
    ; ('('  , TK_L_PAREN)
    ; (')'  , TK_R_PAREN)
    ; ('+'  , TK_ADD)
    ; ('*'  , TK_MUL)
    ; ('='  , TK_ASGN)
    ; ('<'  , TK_LT)
    ; ('\\' , TK_BACKSLASH)
    ]

let msyms = Hashtbl.of_seq @@ List.to_seq
    [ ("->"   , TK_R_ARROW)
    ]
}

let newline = "\n"
let white = ['\t' ' ']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id = (letter | '_') (letter | digit | '_')*
let ssym = ['.' ':' '(' ')' '+' '*' '=' '<' '\\']
let msym = "->"
let int_lit = '-'? digit+
let bool_lit = "true" | "false"

rule tokenize = parse
  | eof             { TK_EOF }
  | white           { tokenize lexbuf }
  | newline         { new_line lexbuf; tokenize lexbuf }
  | "//"            { comment lexbuf }
  | "/*"            { long_comment lexbuf }
  | int_lit   as x  { TK_INT_LITERAL (int_of_string x) }
  | bool_lit  as x  { TK_BOOL_LITERAL (bool_of_string x) }
  | id        as x  { 
                      match Hashtbl.find_opt kws x with
                      | Some token -> token
                      | None       -> TK_ID x
                    }
  | "'" (id as x)   { TK_TID x }
  | msym as op      { Option.get @@ Hashtbl.find_opt msyms op}
  | ssym as op      { Option.get @@ Hashtbl.find_opt ssyms op }
  | _               { raise @@ Error ("unexpected token", lexeme_start_p lexbuf) }
and comment = parse
  | newline     { new_line lexbuf; tokenize lexbuf }
  | _           { comment lexbuf }
and long_comment = parse
  | newline     { new_line lexbuf; long_comment lexbuf }
  | "*/"        { tokenize lexbuf }
  | _           { long_comment lexbuf }
