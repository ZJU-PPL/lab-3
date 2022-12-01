open! Base

(* 表示 fresh 状态的结构，一个计数器 *)
type fresh = { mutable id : int }

let new_fresh () = { id = 0 }

(* 生成新的 fresh 名称
   从 a 到 z, 然后从 t0 开始递增
   本次实验以 'a, 'b, ... 'z, 't0, ... 形式出现
*)
let next_fresh fresh =
  let id = fresh.id in
  fresh.id <- id + 1;
  if id < 26 then String.make 1 @@ Char.of_int_exn @@ (Char.to_int 'a' + id)
  else Printf.sprintf "t%d" (id - 26)
