%{
open Syntax
%}


%start prog
%type <expr> prog

%nonassoc "."
%nonassoc "else"
%nonassoc "in"
%left "<"
%left "+"
%left "*"

%%

prog:
  | e = term "<eof>" { e }

term:
  | e = apply { e }
  | e1 = term op = binop e2 = term { App (App (Name op, e1), e2) }
  | "if" e1 = term "then" e2 = term "else" e3 = term { If (e1, e2, e3) }
  | "let" x = "<id>" "=" e1 = term "in" e3 = term { Let (x, e1, e3) }
  | "\\" x = "<id>" "." e = term { Lam (x, e) }

apply:
  | e = expr { e }
  | e1 = apply e2 = expr { App (e1, e2) }

expr: 
  | x = "<id>"   { Name x }
  | i = "<int>"  { Int i }
  | b = "<bool>" { Bool b }
  | "(" e = term ")" { e }

%inline binop:
  | "<" { "( < )" }
  | "+" { "( + )" }
  | "*" { "( * )" }
