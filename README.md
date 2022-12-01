# PPL Lab3
Note : Currently, only simplified Chinese version(of course, with some academic words in English) is available.

## 实验要求
请在实验前阅读本文件, 以免错过重要信息.
在本实验中, 你将使用 OCaml 实现一个完整的 Hindley-Milner 类型推导算法. 
原则上, 代码文件中的注释足以帮助你完成实验, 如有问题, 请联系我们.

## 项目结构
`lib/` 下的文件:
- `info.ml`
    你的个人信息 **应该** 填写在此处.
- `infer.ml`
    你应该在这里完成所有的类型推导实验内容
- `syntax.ml`
    你可以通过查看这个文件来熟悉本次我们使用的语言
    这个文件还包含很多你可能会用到的打印函数
- `type.ml`
    你可以通过查看这个文件了解本次实验中我们关心的一些结构
    这个文件还包含很多你可能会用到的打印函数
- `fresh.ml`
    用于产生新的类型变量
- `util.ml`
    包含两个实用的运算符，可以将字符串方便的转为类型或者表达式
- `token.mly` `lexer.mll` `parser.mly` `typeparser.mly` `display.ml` 
    这是助教为你提供的帮助函数库, 你**不需要**关心这一部分的代码实现.
    若有需要, 你只需阅读函数名, 并借助 `dune utop` 来查看对应函数的类型.

## 实验组成
实验分为两部分, 其中后一部分依赖前一部分:
1. 简单的类型推导, `infer.ml:Infer_no_let`
2. 完整的带有 `let polymorphism` 的 HM 类型推导 `infer.ml:Infer`

仅就实验指导而言, 你应当按如下顺序阅读文件中的注释:
1. `info.ml` 填写个人信息
2. `syntax.ml` 了解 term 的语法结构
3. `type.ml` 了解 type, substitution, constr, type scheme, context 的结构
4. `infer.ml` 实现类型推导


## 调试指南

本次实验中，你可以使用 `dune exec lab3` 或 `dune test` 来分别以 `bin/main.ml` 或 `test/test.ml` 为入口执行内容。如果你想使用 OCaml Debugger，那么建议使用前者。

### print 法
本次实验提供了大量辅助用的 pretty printer 函数，所以可以有很好的调试效果。这些函数分为两大类，`print_*` 类直接向 `stdout` 进行输出并返回 `unit` 类型；`show_*` 类则返回一个 `string`。
具体而言，你可以直接使用
- `*_ty`: 以自然的格式打印一个类型
- `*_constr`: 以自然的格式打印一个类型约束
- `*_subst`: 以自然的格式打印一个类型替换
- `*_constrs`: 以自然的格式打印类型约束列表
- `*_substs`: 以自然的格式打印类型替换列表
- `*_expr`: 以自然的格式打印一个表达式（其中 lambda abstraction 显示为 `\x. e` 形式）
- `*_expr_in_ocaml`: 以自然的格式打印一个表达式（其中 lambda abstraction 显示为 `fun x -> e` 形式）。如果你不确定一个 expr 的类型，你可以使用这个打印函数，将 expr 的文本形式输入 `utop` 并借助 OCaml 的类型推导器来帮你找出正确的类型。

### debugger 法

此处不提供具体的 OCaml Debugger 使用方法，可以查看[这儿的帮助内容](https://ocaml.org/docs/debugging#the-ocaml-debugger)。到目前为止，OCaml Debugger 也没有与 IDE 和 VSCode 的集成插件，所以也不能完全称得上好用。
为了你方便使用 `ocamldebug`，lab-3 的 bin 部分打开了字节码模式（来防止 dune 对于部分函数符号的修饰）。在你 `dune build` 后，你应该通过 `ocamldebug _build/default/bin/main.bc` 命令来开始调试。


## 分数组成

共有 50 个测试点，每个 2 分，
其中第一部分有 37 个测试点，
第二部分 13 个测试点。
带有 (+) 前缀的为正向测试点，在这些测试点中，你需要推导出一个正确的类型。
带有 (-) 前缀的为反向测试点，在这些测试点中，你需要抛出异常。具体请见 `infer.ml`。
