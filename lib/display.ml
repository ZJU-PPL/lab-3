(* 以下内容可以不看 *)
(* Pretty printer *)
open! Base
open Caml.Format

let print_with_pp f =
  f std_formatter;
  pp_print_newline std_formatter ()

let show_with_pp f =
  f str_formatter;
  flush_str_formatter ()

let print_pos (pos : Lexing.position) =
  Printf.sprintf "%s: line %d, column %d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let pp_list ppf f split xs =
  let rec pp_list_impl f split = function
    | [] -> ()
    | [ x ] -> f ppf x
    | x :: xs ->
        f ppf x;
        split ppf;
        pp_list_impl f split xs
  in
  pp_open_box ppf 2;
  pp_list_impl f split xs;
  pp_close_box ppf ();
  pp_print_flush ppf ()
