open! Base

(* 包含几种你已经熟悉的类型。
   加入了类型变量 TVar，不过这些变量都是使用 next_fresh 自动生成的。
   TVar 在打印时，会显示成 'a, 'b, ..., 详情请看 fresh.ml
   你可以使用 print_ty 来美观地打印出如下形式的 constr
*)
type ty = TBool | TInt | TLam of ty * ty | TVar of string

(* 类型约束。约束是关于类型的方程
   你可以使用 print_constr 来美观地打印出如下形式的 constr
   例如:
    int -> int -> int =~ 'a -> 'b
    'c =~ bool -> bool
    'd =~ 'e
*)
type constr = Constr of ty * ty

(* 类型替换。将一个类型变量替换为另一个类型
   你可以使用 print_subst 来美观地打印出如下形式的 constr
   例如:
    [ 'a => 'b -> 'c ]
    [ 'b => int ]
*)
type subst = Subst of string * ty

(* 类型方案。类似一个含有一阶全称量词的类型
   例如：
    'a 'b . 'a -> 'b
   你可以让这个类型接受两个参数，比如说 int 和 bool
   在进行替换 [ 'a => int ; 'b => bool ] 后
   就可以得到 int -> bool 了
   我们用它来表示多态函数的类型
*)
type tyscheme = Ts of string list * ty

(* 便利的运算符，用于生成清晰的类型约束
   比如可以写 [ TBool =~ t1; t2 =~ TInt ]
*)
let ( =~ ) t1 t2 = Constr (t1, t2)

(* 便利的运算符，用于生成清晰的替换
   比如可以写 [ x => t1 ]
   注意这里的 x 是一个字符串，可以被用在 TVar 中
*)
let ( => ) x t = Subst (x, t)

open Result

(* monadic 风格的类型变量操作函数。
   如果觉得很难理解，那么可以跳过，尽管学会使用这个函数可能会方便写一些代码。
*)
let rec tvarmapM f =
  let ( let* ) = ( >>= ) in
  function
  | TBool -> return TBool
  | TInt -> return TInt
  | TLam (t1, t2) ->
      let* t1 = tvarmapM f t1 in
      let* t2 = tvarmapM f t2 in
      return @@ TLam (t1, t2)
  | TVar s -> f s

(* 给定一个 subst ，将它应用到类型 ty 上（将类型变量替换成目标类型） *)
let subst st ty =
  let (Subst (x, t)) = st in
  Result.ok_exn
  @@ tvarmapM
       (fun s -> if String.equal s x then return t else return @@ TVar s)
       ty

(* 给定一组 subst，将它们依次应用到类型 ty 上。sts 是有序的，不同顺序可能产生不同结果 *)
let rec subst_all sts ty =
  match sts with [] -> ty | st :: rest -> subst_all rest @@ subst st ty

(* 给定一个 subst ，将它应用到 constr 的两侧 *)
let subst_in_constr st (Constr (t1, t2)) = Constr (subst st t1, subst st t2)

(* 给定一组 subst，将它们依次应用到 constr 的两侧。sts 是有序的，不同顺序可能产生不同结果 *)
let rec subst_all_in_constr st cs =
  match cs with
  | [] -> []
  | c :: rest -> subst_in_constr st c :: subst_all_in_constr st rest

(* 类型环境，也就是关联列表
   本次实验会用到两种不同的环境
*)
type 'a ctx = (string * 'a) list

(* 空的环境 *)
let empty = []

(* 查找。找不到会引发异常，尽管对于本次实验来说不需要处理这个异常。
   如果出现异常，则说明你输入的 expr 有误或者你的实现存在问题
*)
let lookup env x =
  match List.Assoc.find env x ~equal:String.equal with
  | Some v -> v
  | None -> failwith @@ "unbound variable " ^ x

(* 用 x ty 拓展类型环境 *)
let extend env x ty = (x, ty) :: env

(* 生成下一个可用的类型变量 *)
let next_fresh_tvar fresh = TVar (Fresh.next_fresh fresh)

(* 这个函数可以将类型变量重命名成从 'a 开始。
   不需要关注实现，也不一定非得使用。
   但是使用它可以让你的结果类型更加赏心悦目。
   请牢记这个函数只能用于最终推导的结果，而不能是中间结果。
*)
let rename ty =
  let fresh = Fresh.new_fresh () in
  let ctx = ref [] in
  Result.ok_exn
  @@ tvarmapM
       (fun s ->
         match List.Assoc.find !ctx s ~equal:String.equal with
         | Some v -> Result.return v
         | None ->
             let n = next_fresh_tvar fresh in
             ctx := (s, n) :: !ctx;
             return n)
       ty

(* 以下内容不用看 *)
(* Pretty printer *)
open Caml.Format

let rec pp_ty ppf = function
  | TVar s -> fprintf ppf "'%s" s
  | TBool -> fprintf ppf "bool"
  | TInt -> fprintf ppf "int"
  | TLam ((TLam _ as tl), t3) ->
      fprintf ppf "@[<2>(%a)@ ->@ %a@]" pp_ty tl pp_ty t3
  | TLam (t1, t2) -> fprintf ppf "@[<2>%a@ ->@ %a@]" pp_ty t1 pp_ty t2

let pp_constr ppf (Constr (t1, t2)) =
  fprintf ppf "@[%a =~ %a@]" pp_ty t1 pp_ty t2

let pp_subst ppf (Subst (x, t)) =
  fprintf ppf "@[[%a |-> %a]@]" pp_ty (TVar x) pp_ty t

let pp_constrs ppf cs =
  Display.pp_list ppf pp_constr (fun ppf -> fprintf ppf ",@ ") cs

let pp_substs ppf cs =
  Display.pp_list ppf pp_subst (fun ppf -> fprintf ppf ",@ ") cs

let print_ty ty = Display.print_with_pp (fun ppf -> pp_ty ppf ty)
let show_ty ty = Display.show_with_pp (fun ppf -> pp_ty ppf ty)
let print_constrs cs = Display.print_with_pp (fun ppf -> pp_constrs ppf cs)
let show_constrs cs = Display.show_with_pp (fun ppf -> pp_constrs ppf cs)
let print_substs sts = Display.print_with_pp (fun ppf -> pp_substs ppf sts)
let show_substs sts = Display.show_with_pp (fun ppf -> pp_substs ppf sts)
let print_constr c = Display.print_with_pp (fun ppf -> pp_constr ppf c)
let show_constr c = Display.show_with_pp (fun ppf -> pp_constr ppf c)
let print_subst st = Display.print_with_pp (fun ppf -> pp_subst ppf st)
let show_subst st = Display.show_with_pp (fun ppf -> pp_subst ppf st)