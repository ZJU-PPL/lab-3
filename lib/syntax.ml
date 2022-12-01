open! Base
(* 这里的 expr 就是前几次 lab 中出现的 term。
   出于简便考虑，在本次实验中我们不使用 nameless 表示法。
   这些 expr 和 OCaml 的表示方法是兼容的，
   稍后你可以看到这一设计如何带来便利。
*)

type expr =
  | Int of int                  (* 一个非负整数，比如 42 *)
  | Bool of bool                (* true 或 false *)
  | Name of string              (* x 或 ( + ), ( * ), ( < )。注意我们将运算符视为普通的二元函数，就像 OCaml 一样 *)
  | If of expr * expr * expr    (* if e1 then e2 else e3 *)
  | Lam of string * expr        (* \x. e 或 fun x -> e *)
  | App of expr * expr          (* e1 e2 *)
  | Let of string * expr * expr (* let x = e1 in e2 *)

(* 以下部分可以不看 *)
(* Pretty printer *)
open Caml.Format

let ident ppf s = fprintf ppf "%s" s
let kwd ppf s = fprintf ppf "%s" s

let pp_expr, pp_expr_in_ocaml =
  let rec pp_lambda ppf x e =
    fprintf ppf "@[<2>(%a%a%a@ %a)@]" kwd "\\" ident x kwd "." pp_expr e
  and pp_lambda_in_ocaml ppf x e =
    fprintf ppf "@[<2>(%a@ %a@ %a@ %a)@]" kwd "fun" ident x kwd "->"
      pp_expr_in_ocaml e
  and pp_expr_aux f ppf =
    let pp_expr = pp_expr_aux f in
    function
    | Int i -> fprintf ppf "%d" i
    | Bool b -> fprintf ppf "%b" b
    | Name n -> fprintf ppf "%a" ident n
    | If (e1, e2, e3) ->
        fprintf ppf "@[<2>%a@ %a@ %a@ %a@ %a@ %a@]" kwd "if" pp_expr e1 kwd
          "then" pp_expr e2 kwd "else" pp_expr e3
    | Lam (x, e) -> f ppf x e
    | App (App (Name "( < )", e1), e2) ->
        fprintf ppf "@[%a@ <@ %a@]" pp_expr e1 pp_expr e2
    | App (App (Name "( + )", e1), e2) ->
        fprintf ppf "@[%a@ +@ %a@]" pp_expr e1 pp_expr e2
    | App (App (Name "( * )", e1), e2) ->
        fprintf ppf "@[%a@ *@ %a@]" pp_expr e1 pp_expr e2
    | App (e1, (App _ as ap)) ->
        fprintf ppf "@[<2>%a@ (%a)@]" pp_expr e1 pp_expr ap
    | App (e1, e2) -> fprintf ppf "@[<2>%a@ %a@]" pp_expr e1 pp_expr e2
    | Let (x, e1, e2) ->
        fprintf ppf "@[<2>%a@ %a@ %a@ %a@ %a@ %a@]" kwd "let" ident x kwd "="
          pp_expr e1 kwd "in" pp_expr e2
  and pp_expr ppf = pp_expr_aux pp_lambda ppf
  and pp_expr_in_ocaml ppf = pp_expr_aux pp_lambda_in_ocaml ppf in
  (pp_expr, pp_expr_in_ocaml)

let print_expr expr =
  pp_expr std_formatter expr;
  pp_print_flush std_formatter ()

let show_expr expr =
  pp_expr str_formatter expr;
  flush_str_formatter ()

let print_expr_in_ocaml expr =
  pp_expr_in_ocaml std_formatter expr;
  pp_print_flush std_formatter ()

let show_expr_in_ocaml expr =
  pp_expr_in_ocaml str_formatter expr;
  flush_str_formatter ()
