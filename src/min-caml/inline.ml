(* インライン展開
 * let rec f x y = x + y in f 1 2
 * -> let rec f x y = x + y in 1 + 2 *)
open KNormal

(* インライン展開する関数の最大サイズ (caml2html: inline_threshold) *)
let threshold = ref 0 (* Mainで-inlineオプションによりセットされる *)

(* 式はいくつの部分式からなるか? *)
let rec size = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2)
  | Let(_, e1, e2) | LetRec({ body = e1 }, e2) -> 1 + size e1 + size e2
  | LetTuple(_, _, e) -> 1 + size e
  | _ -> 1

(* envにはインライン展開できる関数を指す変数とその定義が入る
 * (x, (yts, e1)) ∈ env
 * <-> let rec x yts = e1 in ... という関数定義があり、この関数は十分小さいのでインライン展開できる。 *)
let rec g env = function (* インライン展開ルーチン本体 (caml2html: inline_g) *)
  (* ベースケース *)
  (* - 外部関数をはじくためのwhen節 *)
  | App(x, ys) when M.mem x env -> (* 関数適用の場合 (caml2html: inline_app) *)
    let (zts, e) = M.find x env in
    Format.eprintf "inlining %s@." x;
    (* インライン化の後はアルファ変換が必要になる *)
    let env' =
      (* 関数の引数から実際に適用される値への写像を作る *)
      List.fold_left2
        (fun env' (z, _) y -> M.add z y env')
        M.empty
        zts
        ys in
    Alpha.g env' e
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* 関数定義の場合 (caml2html: inline_letrec) *)
    (* 関数が十分小さいなら、envに入れる *)
    let env = if size e1 > !threshold then env else M.add x (yts, e1) env in
    LetRec({ name = (x, t); args = yts; body = g env e1}, g env e2)

  (* 再帰ステップ *)
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | Let(xt, e1, e2) -> Let(xt, g env e1, g env e2)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
  | e -> e

let f e = g M.empty e
