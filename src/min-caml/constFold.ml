(* 定数畳み込みを行う *)
(* let x = 1 + 2 in let 4 + 5 in ...
   -> let x = 3 in let y = 9 in ...  *)

open KNormal

(* xがenv中でInt型だったらtrue *)
let memi x env =
  try (match M.find x env with Int(_) -> true | _ -> false)
  with Not_found -> false
(* xがenv中でFloat型だったらtrue *)
let memf x env =
  try (match M.find x env with Float(_) -> true | _ -> false)
  with Not_found -> false
(* xがenv中でTuple型だったらtrue *)
let memt x env =
  try (match M.find x env with Tuple(_) -> true | _ -> false)
  with Not_found -> false

(* xがenv中で{Int, Float, Tuple}型だったらその値を返す *)
let findi x env = (match M.find x env with Int(i) -> i | _ -> raise Not_found)
let findf x env = (match M.find x env with Float(d) -> d | _ -> raise Not_found)
let findt x env = (match M.find x env with Tuple(ys) -> ys | _ -> raise Not_found)

(* envに含まれる変数に関する情報を用いて、gに含まれる定数からなる式を定数に置き換える *)
let rec g env = function (* 定数畳み込みルーチン本体 (caml2html: constfold_g) *)
  (* ベースケース *)
  | Var  (x)            when memi x env -> Int   (findi x env)
  (* | Var(x) when memf x env -> Float(findf x env) *)
  (* | Var(x) when memt x env -> Tuple(findt x env) *)
  | Neg  (x)            when memi x env -> Int   (- (findi x env))
  | FNeg (x)            when memf x env -> Float (-.(findf x env))
  | Add  (x, y)         when memi x env && memi y env -> Int   (findi x env +  findi y env) (* 足し算のケース (caml2html: constfold_add) *)
  | Sub  (x, y)         when memi x env && memi y env -> Int   (findi x env -  findi y env)
  | FAdd (x, y)         when memf x env && memf y env -> Float (findf x env +. findf y env)
  | FSub (x, y)         when memf x env && memf y env -> Float (findf x env -. findf y env)
  | FMul (x, y)         when memf x env && memf y env -> Float (findf x env *. findf y env)
  | FDiv (x, y)         when memf x env && memf y env -> Float (findf x env /. findf y env)
  | IfEq (x, y, e1, e2) when memi x env && memi y env -> if findi x env =  findi y env then g env e1 else g env e2
  | IfEq (x, y, e1, e2) when memf x env && memf y env -> if findf x env =  findf y env then g env e1 else g env e2
  | IfLE (x, y, e1, e2) when memi x env && memi y env -> if findi x env <= findi y env then g env e1 else g env e2
  | IfLE (x, y, e1, e2) when memf x env && memf y env -> if findf x env <= findf y env then g env e1 else g env e2

  (* 再帰ステップ *)
  (* - Let式の場合は環境を更新が必要 *)
  | Let((x, t), e1, e2) -> (* letのケース (caml2html: constfold_let) *)
    let e1' = g env e1 in
    let e2' = g (M.add x e1' env) e2 in
    Let((x, t), e1', e2')
  | LetRec({ name = x; args = ys; body = e1 }, e2) ->
    LetRec({ name = x; args = ys; body = g env e1 }, g env e2)
  | LetTuple(xts, y, e) when memt y env ->
    List.fold_left2
      (fun e' xt z -> Let(xt, Var(z), e'))
      (g env e)
      xts
      (findt y env)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
  (* - 簡略化できないif文 *)
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)

  (* その他: そのまま *)
  | e -> e

let f = g M.empty
