open! Base

(* 前缀运算符，将一个表示类型的字符串转换为类型 *)
let ( ?~ ) s =
  let buf = Lexing.from_string s in
  try Typeparser.ty Lexer.tokenize buf with
  | Parser.Error ->
      failwith
      @@ Printf.sprintf "Parser failed: %s\n%!"
      @@ Display.print_pos (Lexing.lexeme_start_p buf)
  | Lexer.Error (_cause, location) ->
      failwith
      @@ Printf.sprintf "Lexer failed: %s\n%!"
      @@ Display.print_pos location

(* 前缀运算符，将一个表示 expr 的字符串转换为 expr *)
let ( ?@ ) s =
  let buf = Lexing.from_string s in
  try Parser.prog Lexer.tokenize buf with
  | Parser.Error ->
      failwith
      @@ Printf.sprintf "Parser failed: %s\n%!"
      @@ Display.print_pos (Lexing.lexeme_start_p buf)
  | Lexer.Error (_cause, location) ->
      failwith
      @@ Printf.sprintf "Lexer failed: %s\n%!"
      @@ Display.print_pos location
