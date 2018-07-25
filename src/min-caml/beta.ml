(* ベータ簡約 *)
(* let x = 1 in let y = x in let z = y in x + y + z
   -> let x = 1 in x + x + x  *)

open KNormal

(* 環境envのもとでxの置換を試みる。置換できなければそのまま。 *)
let find x env = try M.find x env with Not_found -> x (* 置換のための関数 (caml2html: beta_find) *)

(* gにベータ簡約を行う
 * (x1, x2) ∈ env <-> x1とx2は同じ値を指しているので、互いに置換可能 *)
let rec g env = function (* β簡約ルーチン本体 (caml2html: beta_g) *)
  (* ベースケース *)
  | Let((x, t), e1, e2) -> (* letのβ簡約 (caml2html: beta_let) *)
    (match g env e1 with
     (* let x = y in ... のとき: xの定義を削除し、envにxが出てきたらyに置換せよという情報を追加する *)
     | Var(y) ->
       Format.eprintf "beta-reducing %s = %s@." x y;
       g (M.add x y env) e2
     | e1' ->
       (* そうでない場合はただの再帰的適用 *)
       let e2' = g env e2 in
       Let((x, t), e1', e2'))
  (* - envに対応付けが記録されていれば、それで置き換える *)
  | Var(x) -> Var(find x env) (* 変数を置換 (caml2html: beta_var) *)
  (* 再帰ステップ *)
  | Unit -> Unit
  | Int(i) -> Int(i)
  | Float(d) -> Float(d)
  | Neg(x) -> Neg(find x env)
  | Add(x, y) -> Add(find x env, find y env)
  | Sub(x, y) -> Sub(find x env, find y env)
  | FNeg(x) -> FNeg(find x env)
  | FAdd(x, y) -> FAdd(find x env, find y env)
  | FSub(x, y) -> FSub(find x env, find y env)
  | FMul(x, y) -> FMul(find x env, find y env)
  | FDiv(x, y) -> FDiv(find x env, find y env)
  | IfEq(x, y, e1, e2) -> IfEq(find x env, find y env, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(find x env, find y env, g env e1, g env e2)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
    LetRec({ name = xt; args = yts; body = g env e1 }, g env e2)
  | Tuple(xs) -> Tuple(List.map (fun x -> find x env) xs)
  | LetTuple(xts, y, e) -> LetTuple(xts, find y env, g env e)
  | Get(x, y) -> Get(find x env, find y env)
  | Put(x, y, z) -> Put(find x env, find y env, find z env)
  | App(g, xs) -> App(find g env, List.map (fun x -> find x env) xs)
  | ExtArray(x) -> ExtArray(x)
  | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)

let f = g M.empty
