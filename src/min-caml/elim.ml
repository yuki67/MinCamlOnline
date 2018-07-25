(* 不要式削除
 * let x = 1 in let y = 2 in let z = 3 in x + z
 * -> let x = 1 in let z = 3 in x + z *)

open KNormal

(* 式は副作用を持つか? *)
let rec effect = function (* 副作用の有無 (caml2html: elim_effect) *)
  | App _ | Put _ | ExtFunApp _ -> true
  | LetRec(_, e)
  | LetTuple(_, _, e) -> effect e
  | Let(_, e1, e2)
  | IfEq(_, _, e1, e2)
  | IfLE(_, _, e1, e2) -> effect e1 || effect e2
  | _ -> false

let rec f = function (* 不要定義削除ルーチン本体 (caml2html: elim_f) *)
  (* ベースケース *)
  | Let((x, t), e1, e2) -> (* letの場合 (caml2html: elim_let) *)
    let e1' = f e1 in
    let e2' = f e2 in
    if effect e1' || S.mem x (fv e2') (* xの定義式が副作用を持つか、xがe2中に出現するなら、 *)
    then Let((x, t), e1', e2')        (* そのまま *)
    else                              (* そうでなければ定義を削除 *)
      (Format.eprintf "eliminating variable %s@." x;
       e2')
  (* Elim.fと名前が衝突するので、LetRec式で定義される関数の名前をfではなくxに束縛する *)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recの場合 (caml2html: elim_letrec) *)
    let e2' = f e2 in
    if S.mem x (fv e2') (* xがe2中に出現するならそのまま、そうでなければ削除 *)
    then LetRec({ name = (x, t); args = yts; body = f e1 }, e2')
    else
      (Format.eprintf "eliminating function %s@." x;
       e2')
  | LetTuple(xts, y, e) ->
    let xs = List.map fst xts in
    let e' = f e in
    let live = fv e' in
    if List.exists (fun x -> S.mem x live) xs
    then LetTuple(xts, y, e') (* 定義される変数で、eの中で生きているものが一つでもあるならそのまま *)
    else                      (* どれも使われるないなら削除 *)
      (Format.eprintf "eliminating variables %s@." (Id.pp_list xs);
       e')

  (* 再帰ステップ *)
  | IfEq(x, y, e1, e2) -> IfEq(x, y, f e1, f e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, f e1, f e2)
  | e -> e
