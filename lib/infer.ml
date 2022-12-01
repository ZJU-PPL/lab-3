open! Base
open Type
open Syntax
open Util
open Fresh

(* 在本次 lab 中大家需要实现 Hindley-Milner 类型推导算法，
   这一算法及其变体广泛应用于 ML 系家族语言 ( OCaml, SML, Haskell 等 )
   也是这些语言较少需要写类型标注的原因。
   实验分为了两个部分：
   第一个部分为我们定义在 syntax.ml 里的语言减去 let 表达式实现类型推导。
   第二个部分为我们定义在 syntax.ml 里的语言实现类型推导。
   本次实验所有需要你编写的部分都在这个文件里。尽管如此，你可能还需要阅读 syntax.ml type.ml 中的信息
*)

[@@@warning "-27"]
[@@@warning "-39"]

(* 适时抛出这个异常来表示推导失败（可能是由于输入了无法通过类型检查的 expr） *)
exception UnificationFailed of constr

(* 所有抛出这个异常的地方都应该被替换成你自己的代码 *)
exception Todo

(* 第一部分：为没有 let 的 expr 实现类型推导 *)
module Infer_no_let = struct
  (* 在需要处理 let 的地方抛出这个异常 *)
  exception Unimplemented

  (* 我们需要处理的原始语言是这个样子的：
      e ::= n | i | b                       (* i, b 分别代表整数和布尔值 true, false *)
          | if e1 then e2 else e3
          | \x. e
          | e1 e2

      n ::= x | ( + ) | ( * ) | ( <= )      (* 注意运算符被当作二元函数了。这部分的处理不需要由你完成 *)
      t ::= int | bool | t1 -> t2

     可以看到我们并没有像 lab-2 一样提供类型标注的位置，
     这意味着如果我们需要推断出 expr 的类型。

     类型推导的思路是什么？如果是你，你会如何为没有类型标注的表达式推导类型？
     直觉上看，对于 \x. 2 + x （等价于 \x. ( + ) 2 x)
     - 我们还不知道 x 的类型，让我们认为它是未知类型 t
     - 我们已经知道 ( + ) 具有 int -> int -> int 类型了
     - 我们将 ( + ) 首先应用到 2 上，而显然 2 具有 int 类型，结果类型是 int -> int
     - 由于具有 int -> int 类型的结果被应用到 x 上，我们得到了一个约束 t =~ int （也可以是 int =~ t，顺序并不要紧）
     - 再次应用的结果是 int，因此这个 lambda 表达式返回类型是 int
     - 因此这个 lambda 表达式的类型是 t -> int，而我们已经有 t =~ int 的约束了
     - 所以我们推导出这个表达式的类型是 int -> int

     这个过程不仅符合我们的心智模型，还体现了 HM 算法中三个关键步骤
     - 生成未知的类型
     - 收集约束
     - 求解约束
  *)

  (* 为了实现类型推导，我们需要一个类型环境 ty ctx。注意这里的 ctx 只是一个 list，
     你可以使用提供的 lookup, extend, empty 来操作它。
     在正式开始前，请先写出 ( * ) 和 ( < ) 的类型。
     应当认为他们只对 int 进行运算，产生不同类型的结果。
     使用 ?~ 可以将字符串转换成对应的类型，你也可以手动编写这些类型
     例如:
      ?~"int -> int -> int" === TLam (TInt, TLam (TInt, TInt))
  *)
  let (initial_ctx : ty ctx) =
    let ctx = empty in
    let ctx = extend ctx "( + )" @@ ?~"int -> int -> int" in
    let ctx = extend ctx "( * )" @@ ?~"int" (* Todo *) in
    let ctx = extend ctx "( < )" @@ ?~"int" (* Todo *) in
    ctx

  (* 回顾我们的目标语言：
      e ::= n | i | b
          | if e1 then e2 else e3
          | \x. e
          | e1 e2

      n ::= x | ( + ) | ( * ) | ( <= )
      t ::= int | bool | t1 -> t2

     让我们先开始严谨地描述这个算法的逻辑：
     首先我们需要引入一个四元关系： ctx |- e : t -| C，
     这个关系的含义是，在 ctx 中，e 被推断具有 t 类型，并产生了约束集合 C。

     在我们的实现中，e 表示为 expr，t 表示为 ty，C 表示为 constr list。
     如果你还不熟悉这些类型，请先按顺序前往 syntax.ml 和 type.ml 查看。
  *)

  (* collect 函数用于收集约束并返回一个含有未知类型变量的类型。
     它的编写完全依照上文提到的四元推导关系
     具体而言，它接受 ctx 和 e （实际实现中还需要一个 fresh），给出 t 和 C
  *)
  let rec collect fresh ctx expr : ty * constr list =
    match expr with
    (* 可以很简单地写出常量的的推导关系。没有要生成的约束，也没引入新的未知类型
            (nothing)
       --------------------
        ctx |- i: int  -| {}

            (nothing)
       --------------------
        ctx |- b: bool -| {}
    *)
    | Int _ -> raise Todo
    | Bool _ -> raise Todo
    (* 推导一个名字的类型只需要我们在环境 ctx 中进行查找即可。没有要生成的约束，也没引入新的未知类型
            (nothing)
       --------------------
        ctx |- n: lookup ctx n -| {}
    *)
    | Name _ -> raise Todo
    (* 接下来的规则相对复杂一点，我们会加入新的约束
        ctx |- e1 : t1 -| C1
        ctx |- e2 : t2 -| C2
        ctx |- e3 : t3 -| C3
       --------------------
        ctx |- if e1 then e2 else e3 : t2 -| C1, C2, C3, t1 =~ TBool, t2 =~ t3

        注意到我们收集了条件，then 和 else 部分的约束，还额外加入了 t1 =~ TBool 和 t2 =~ t3 的约束
        加入的约束相当符合我们的直觉
    *)
    | If (e1, e2, e3) ->
        let t1, c1 = collect fresh ctx e1 in
        let t2, c2 = collect fresh ctx e2 in
        let t3, c3 = collect fresh ctx e3 in
        (t2, c1 @ c2 @ c3 @ [ t1 =~ TBool; t2 =~ t3 ])
    (* lambda 表达式需要引入新的类型变量 t1，它是未知的。
         我们在确定 e 的类型时，可能会使用到 t1，
         并因此生成一些约束，通过 C 传递
        t1 is fresh
        ctx, x : t1 |- e : t2 -| C
       --------------------
        ctx |- \x. e : t1 -> t2 -| C

         你可以通过 next_fresh_tvar 来生成新的类型变量
    *)
    | Lam (_, _) -> raise Todo
    (* 应用的规则如下。
         可能有人会产生疑惑，
         为什么我们不能直接将 t1 解开成为形如 ta -> tb 的 TLam 类型，
         然后生成 t2 =~ ta 的约束呢？
         这是因为此时 t1 可能仍然是未知的类型，即便它确实是 TLam，
         这个事实也要等到我们收集完约束后的约束求解阶段才可以得知。
         所以我们为结果类型视为未知类型，并生成对应的新的类型变量

        ctx |- e1 : t1 -| C1
        ctx |- e2 : t2 -| C2
        t3 is fresh
       --------------------
        ctx |- e1 e2: t3 -| C1, C2, t1 =~ t2 -> t3

         你可以通过 next_fresh_tvar 来生成新的类型变量
    *)
    | App (_, _) -> raise Todo
    (* 暂时不需要实现这一部分
    *)
    | Let _ -> raise Unimplemented

  (* 确定类型 t 中是否含有一个叫做 x 的类型变量。这个函数知道用法即可，在 unify 里面会用到 *)
  let contains t x : bool =
    Result.is_error
    @@ tvarmapM
         (fun s ->
           if String.equal s x then Result.fail () else Result.return @@ TVar s)
         t

  (* 约束求解的过程被称作 unification，
     这个过程将通过尽量替换类型变量，来使约束的两侧变得一致。

     在本次实验中，类型替换使用 subst 来表示。
     你可以通过 x => t 来构造一个将 TVar x 替换为 t 的替换。
     类型替换可以作用于约束 constr 与类型 ty。
     例如：
       对于约束：
        (t1 =~ t2) [x => t] === (t1 [x => t]) =~ (t2 [x => t])
       对于类型：
        int [x => t] === int
        bool [x => t] === bool
        x [x => t] === t
        y [x => t] === y
        (t1 -> t2) [x => t] === (t1 [x => t]) -> (t2 [x => t])

     我们允许替换的复合。例如，t [x => t; y => t'] 等价于 (t [x => t]) [y => t']
  *)

  (* 我们称一组替换 S 统一 (unify) 了约束 t1 =~ t2，
     当且仅当 (t1 S) === (t2 S)，
     而约束集合求解的过程，就是给定 C，求 S 的过程
     在实现正确的情况下，如果无法找到满足要求的 S，则说明 C 所对应的 expr 无法通过类型检查
     例如：
        约束为 'x -> ('x -> int) =~ int -> 'y
        那么 ['y => int -> int; 'x -> int] 就可以将其统一
  *)
  let rec unify (cs : constr list) : subst list =
    match cs with
    (* unify 算法的描述如下：
       - 如果 C 为空，则返回空的替换
    *)
    | [] -> []
    (*
        - 如果 C 含有至少一个约束 t1 =~ t2，剩余约束为 C'
          + t1, t2 都是相同简单类型，比如 int, int，则结果就是 unify C'

          + t1, t2 都是相同的类型变量，比如 'x, 'x，则结果就是 unify C'

            (* OCaml 小提示： 在使用模式匹配时，你可以写（注意 when 对模式进行了进一步的限制）
              match t1, t2 with
              | TVar x, TVar y when String.equal x y -> ...
              | _ -> ... 
            *)

          + t1 是任意一个类型变量 'x, 且 t1 不在 t2 中出现，则生成替换
            S = 'x => t2，并将替换应用到 C' 上。结果是 S; unify (C' S)

          + t2 是任意一个类型变量 'x, 且 t2 不在 t1 中出现，则生成替换
            S = 'x => t1，并将替换应用到 C' 上。结果是 S; unify (C' S)

          + t1 = i1 -> o1 且 t2 = i2 -> o2，则结果是 unify(i1 =~ i2, o1 =~ o2, C')

          + 否则代表求解失败，这个时候请抛出一个 UnificationFailed 异常并带上产生异常的约束
    *)
    | (Constr (_t1, _t2) as _c) :: _rest -> raise Todo

  (* 然后我们就可以进行完整的类型推导了：
      先收集约束集 cs，并得到一个类型 t
      然后求解约束集合得到一组替换 sts
      最后将 sts 应用到 t 上得到最终推导得到的类型。
      你可以在 test/test.ml 中测试这个函数
  *)
  let infer ctx expr : ty =
    let fresh = new_fresh () in
    let t, cs = collect fresh ctx expr in
    let sts = unify cs in
    subst_all sts t |> rename
end

(* 第二部分：为含有 let 的 expr 实现类型推导
   这一部分由于 let 多态的问题而变得微妙起来。
*)
module Infer = struct
  (* 我们需要处理的语言是这个样子的：
      e ::= n | i | b
          | if e1 then e2 else e3
          | \x. e
          | e1 e2
          | let x = e1 in e2 (* 新的 *)

      n ::= x | ( + ) | ( * ) | ( <= )
      t ::= int | bool | t1 -> t2

     对于新的 let 语法，也许我们可以轻松地根据写出以下的推导关系
       ctx |- e1: t1 -| C1
       ctx, x : t1 |- e2 : t2 -| C2
       --------------------
       ctx |- let x = e1 in e2 : t2  -| C1, C2

     但是我们可能会发现，这样的约束过于严格了。
     例如，在
        let id = \x. x in
        let a = id 0 in
        id true
     中，我们可以为 id 推导出 'a -> 'a 的类型。
     但是在使用 id 0 后，我们将会收集到一个 'a -> 'a =~ int -> 'b 的约束，
     而随后我们又使用了 id true，我们还会得到一个 'a -> 'a =~ bool -> 'c 的约束。
     在约束求解阶段，这些约束被分解为更小的 'a =~ int; 'a =~ 'b; 'a =~ bool; 'a =~ 'c
     而显然不存在替换能够统一 'a =~ int 和 'a =~ bool !

     可见，上面的严格推导关系破坏了我们想要的函数多态性。
     在 let 不存在时，这样的问题不会出现，
     因为我们在多次使用同一个函数时总是要重复一遍它的定义的（不考虑 Y 和 Z 组合子：他们是无法通过类型检查的），
     这样就可以为不同处的函数定义产生不同的约束和类型变量并最终得到不同的类型了。

     但是我们引入 let 不正是想要避免重复定义吗？
     为此，在 HM 类型系统中引入了 let 多态性，要实现这一点，我们需要一种新的类型，
     我们称之为类型方案。
  *)

  (* 我们使用几乎相同的类型环境，但是不同的是这次我们环境中存储的不是一个个类型而是类型方案
     我们使用 tyscheme，也就是 Ts (string list, ty) 来表示它
     类型方案是含有一阶全称量词的类型
     例如：
      'a 'b . 'a -> 'b === Ts (["a", "b"], TLam (TVar "a", TVar "b"))
     你可以让这个类型接受两个参数，比如说 int 和 bool
     在进行替换 [ 'a => int ; 'b => bool ] 后
     就可以得到 int -> bool 了
     我们用它来表示多态函数的类型 *)
  let initial_ctx =
    List.map ~f:(fun (x, t) -> (x, Ts ([], t))) Infer_no_let.initial_ctx

  (* unify 函数和 contains 函数可以不用修改 *)
  let unify = Infer_no_let.unify
  let contains = Infer_no_let.contains

  (* 为了适应新的类型方案，我们需要一些新的函数，instantiate 和 generalize
     instantiate 负责将一个 tyscheme 实例化为一个 ty，
     这对应着修改后的推导形式：
                (nothing)
      --------------------
      ctx |- n: instantiate (lookup ctx n) -| {}

     仍然是这个例子
        let id = \x. x in
        let a = id 0 in
        id true
     我们首先为 id 推导出 'a . 'a -> 'a 类型 （也就是对于任意类型 'a, id 的类型是 'a -> 'a)。
     ** 怎么推导出的请看之后关于 generalize 的部分 **

     每次使用 id 时，我们需要实例化 id 的 ts。
     第一次使用，这里的 'a 会被替换成一个 fresh 的变量例如 'b
     因此我们得到 'b -> 'b，再由于应用规则，我们得到约束 'b -> 'b =~ int -> 'c

     第二次有 id true，我们得到另一个 'd -> 'd =~ bool -> 'e 的约束。

     可以看到，新的约束不会产生冲突，这是由于我们每次使用的 id 的类型都是从 id 的类型方案实例化出的新的类型，
     从而多次使用时，这些使用之间不会产生联系而互相限制。
  *)

  (* 现在请你写出一个可用的 instantiate 函数
     因为要生成新的类型变量，所以还需要一个 fresh 作为参数，使用 next_fresh_tvar 生成
     你需要检查 Ts (xs, t) 中的 xs，只有名称位于 xs 中的的 tvar 才是需要被替换的！
     举例而言，'a. 'a -> 'b 在实例化时，我们应该将 'a 替换成崭新的类型变量，而不替换 'b
  *)
  let instantiate fresh (Ts (xs, t)) : ty = raise Todo

  let rec collect fresh ctx expr : ty * constr list =
    match expr with
    (* 照旧 *)
    | Int _ -> raise Todo
    | Bool _ -> raise Todo
    (* 请依据上文中提到的新的推导关系修改这里的代码 *)
    | Name x -> raise Todo
    (* 照旧 *)
    | If (e1, e2, e3) ->
        let t1, c1 = collect fresh ctx e1 in
        let t2, c2 = collect fresh ctx e2 in
        let t3, c3 = collect fresh ctx e3 in
        (t2, c1 @ c2 @ c3 @ [ t1 =~ TBool; t2 =~ t3 ])
    (* 这里有一些 trivial 的修改，因为我们的类型环境 ctx 存储的内容发生了变化，
       从类型变成了类型方案。
       对于 x，我们需要加入列表为空的 type scheme
    *)
    | Lam (x, e) -> raise Todo
    (* 照旧 *)
    | App (e1, e2) -> raise Todo
    (* 前面提到，在这个例子
        let id = \x. x in
        let a = id 0 in
        id true
       我们可以为 id 推导出 'a . 'a -> 'a 的类型方案，
       这是如何做到的？对 \x. x 直接会推导出 'a -> 'a 类型。
       这一过程我们称之为泛化，也就是 generalize。

       严格的推导关系 (x)
        ctx |- e1: t1 -| C1
        ctx, x : t1 |- e2 : t2 -| C2
        --------------------
        ctx |- let x = e1 in e2 : t2  -| C1, C2

       合适的推导关系
        ctx |- e1: t1 -| C1
        generalize ctx x t1 C1 |- e2 : t2 -| C2
        --------------------
        ctx |- let x = e1 in e2 : t2 -| C2

        请在这里将代码补充完全，然后转到之后 generalize 的实现上
    *)
    | Let (x, e1, e2) -> raise Todo

  (* 考虑
        ctx |- e1: t1 -| C1
        generalize ctx x t1 C1 |- e2 : t2 -| C2
        --------------------
        ctx |- let x = e1 in e2 : t2 -| C2

     为了 generalize t1，我们需要怎么做？

     注意到 t1 可能是一个未知的类型变量（比如 'a )，
     我们想要在泛化前尽量使这个类型变得具体。
     抛掉 let 的其他部分，假设 e1 是整个程序，我们是如何推导 e1 的类型的？
     参考之前 infer 的实现，在 collect e1 得到 t1, C1 后，我们应该 unify C1，得到一组替换 S
     然后将 S 应用到 t1 上，从而提前推导出 e1 的类型 u。
     ** 重要：我们提前对一部分类型变量进行了替换，为了使整个推导一致，
     我们也要对此时的 ctx 中所有的类型方案进行替换，得到 ktx **

     现在的问题就是 u 中有哪些类型变量应该被泛化了。我们不能泛化所有的类型变量。
     考虑
      (\x. (let y = \z. x in
             let w = y true in
             y 0))
     当我们处理 let 时，如果 ctx 中已经有 x |-> 'a 了，我们不应该泛化 'a，也就是说
     y 应该被推导为 'b. 'b -> 'a 而不是 'a 'b. 'b -> 'a，
     否则这意味着每次使用 y，我们都会给其中的 x 赋予一个新的类型！

     所以我们应该只泛化那些出现在 u 中而没有出现在 ktx 中的类型变量。

     综合而言，
      generalize ctx x t c = extend ktx x ('a1 ... 'an . u)
      其中 'a1 ... 'an 出现在 u 中但没有出现在 ktx 中
  *)
  and generalize ctx (x : string) (t : ty) (c : constr list) : tyscheme ctx =
    raise Todo

  (* 这个函数将一系列替换作用到 ctx 上，你可能需要在 generalize 中使用它 *)
  (* 对于每个类型方案，我们不需要考虑类型参数列表的替换，
     这是因为出现在列表中的类型变量，在用到时必然会被实例化从而转为新的类型变量，
     也就是说必定不可能在之后被 **直接** 使用，也就不可能被之后的过程替换了 *)
  and subst_all_in_ctx sts ctx =
    List.map ~f:(fun (x, Ts (xs, t)) -> (x, Ts (xs, subst_all sts t))) ctx

  (* 一样的推导函数 *)
  let infer ctx expr : ty =
    let fresh = new_fresh () in
    let t, cs = collect fresh ctx expr in
    let sts = unify cs in
    subst_all sts t |> rename
end
