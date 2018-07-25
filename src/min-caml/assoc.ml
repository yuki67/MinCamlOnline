(* flatten let-bindings (just for prettier printing) *)
(* let x = let y = 2 in y + 1 in x
   -> let y = 2 in let x = y + 1 in x  *)

open KNormal

let rec f = function (* ネストしたletの簡約 (caml2html: assoc_f) *)
  (* ベースケース *)
  | Let(xt, e1, e2) -> (* letの場合 (caml2html: assoc_let) *)
    (* 連続するLetの一番最後に let xt = e1' in e2' を入れる *)
    let rec insert = function
      | Let(yt, e3, e4) -> Let(yt, e3, insert e4)
      | LetRec(fundefs, e) -> LetRec(fundefs, insert e)
      | LetTuple(yts, z, e) -> LetTuple(yts, z, insert e)
      | e -> Let(xt, e, f e2) in
    insert (f e1)
  (* 再帰ステップ *)
  | IfEq(x, y, e1, e2) -> IfEq(x, y, f e1, f e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, f e1, f e2)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
    LetRec({ name = xt; args = yts; body = f e1 }, f e2)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, f e)
  | e -> e
