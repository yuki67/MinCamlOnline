(* type inference/reconstruction *)

open Syntax

(* raise Unify(t1, t2) <-> t1とt2は同じ型を表しているはずだが、t1とt2に現れる
 *                         型変数をどう解釈しても同じ型を表すことができない *)
exception Unify of Type.t * Type.t

(* 外部変数とみなされた型変数からその型へのMap *)
let extenv = ref M.empty

(* 値が確定した型変数に対してのみtrue *)
let is_fixed = function
  | Type.Var { contents = Some _ } -> true
  | _ -> false

(* for pretty printing (and type normalization) *)
(* 引数の型に含まれる型変数を実際の型で書き換えることで、型から型変数を取り除く。
 *  ただし、
 *  - 型変数が確定済ならば、その型で置き換える
 *  - 型変数が未確定ならば、その型をIntとみなした上でIntに置き換える *)
let rec deref_typ = function (* 型変数を中身でおきかえる関数 (caml2html: typing_deref) *)
  (* ベースケース *)
  | Type.Var({ contents = None } as r) ->
    (* 型変数がどの型か決まっていない *)
    Format.eprintf "uninstantiated type variable detected; assuming int@.";
    r := Some(Type.Int);
    Type.Int
  | Type.Var({ contents = Some(t) } as r) ->
    (* 型変数がどの型か決まっている *)
    let t' = deref_typ t in
    r := Some(t');
    t'
  (* 再帰ステップ *)
  | Type.Fun(t1s, t2) -> Type.Fun(List.map deref_typ t1s, deref_typ t2)
  | Type.Tuple(ts)    -> Type.Tuple(List.map deref_typ ts)
  | Type.Array(t)     -> Type.Array(deref_typ t)
  (* 型変数を含まない型はそのまま *)
  | t -> t

let rec deref_id_typ (x, t):(Id.t * Type.t) = (x, deref_typ t)

(* Let式で定義される変数の型から型変数を取り除く *)
let rec deref_term = function
  (* ベースケース *)
  | Let(xt, e1, e2) -> Let(deref_id_typ xt, deref_term e1, deref_term e2)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
    LetRec({ name = deref_id_typ xt;
             args = List.map deref_id_typ yts;
             body = deref_term e1 },
           deref_term e2)
  | LetTuple(xts, e1, e2) -> LetTuple(List.map deref_id_typ xts, deref_term e1, deref_term e2)
  (* 再帰ステップ *)
  | Not   (e)          -> Not   (deref_term e)
  | Neg   (e)          -> Neg   (deref_term e)
  | Add   (e1, e2)     -> Add   (deref_term e1, deref_term e2)
  | Sub   (e1, e2)     -> Sub   (deref_term e1, deref_term e2)
  | Eq    (e1, e2)     -> Eq    (deref_term e1, deref_term e2)
  | LE    (e1, e2)     -> LE    (deref_term e1, deref_term e2)
  | FNeg  (e)          -> FNeg  (deref_term e)
  | FAdd  (e1, e2)     -> FAdd  (deref_term e1, deref_term e2)
  | FSub  (e1, e2)     -> FSub  (deref_term e1, deref_term e2)
  | FMul  (e1, e2)     -> FMul  (deref_term e1, deref_term e2)
  | FDiv  (e1, e2)     -> FDiv  (deref_term e1, deref_term e2)
  | If    (e1, e2, e3) -> If    (deref_term e1, deref_term e2, deref_term e3)
  | App   (e, es)      -> App   (deref_term e, List.map deref_term es)
  | Tuple (es)         -> Tuple (List.map deref_term es)
  | Array (e1, e2)     -> Array (deref_term e1, deref_term e2)
  | Get   (e1, e2)     -> Get   (deref_term e1, deref_term e2)
  | Put   (e1, e2, e3) -> Put   (deref_term e1, deref_term e2, deref_term e3)
  (* 型変数を含まない式はそのまま *)
  | e -> e

(* orrur r t <-> 型tの中に"Var r"が出現する *)
let rec occur r = function (* occur check (caml2html: typing_occur) *)
  (* ベースケース *)
  | Type.Var   (r') when r == r'        -> true
  | Type.Var   ({ contents = None })    -> false
  (* 再帰ステップ *)
  | Type.Fun   (ts, t)                  -> List.exists (occur r) ts || occur r t
  | Type.Tuple (ts)                     -> List.exists (occur r) ts
  | Type.Array (t)                      -> occur r t
  | Type.Var   ({ contents = Some(t) }) -> occur r t
  (* 型変数が出てこないので"Var r"も出現しない *)
  | _ -> false

(* t1とt2が同じ型を持つ、という仮定をおいたとき、
 *   - 矛盾が起きるか? (起きるならraise Unify (t1, t2))
 *   - t1, t2が持つ型変数は決定するか? (決定するものは型を代入する) *)
let rec unify t1 t2 = (* 型が合うように、型変数への代入をする (caml2html: typing_unify) *)
  match t1, t2 with
  (* ベースケース *)
  (* - 型変数を含まない場合、無条件でパス *)
  | Type.Unit, Type.Unit | Type.Bool,  Type.Bool
  | Type.Int,  Type.Int  | Type.Float, Type.Float -> ()
  (* - 物理的に等しい型変数ならパス *)
  | Type.Var r1, Type.Var r2 when r1 == r2 -> ()
  (* - 片方が未確定の型変数の場合、値を確定させる *)
  (* - もう片方が確定した型変数の場合は再帰ステップなので除外 *)
  | Type.Var({ contents = None } as r), t when not (t |> is_fixed) ->
    if occur r t then raise (Unify(t1, t2));
    r := Some(t)
  | t, Type.Var({ contents = None } as r) when not (t |> is_fixed) ->
    if occur r t then raise (Unify(t1, t2));
    r := Some(t)
  (* 再帰ステップ *)
  | Type.Var { contents = Some t1' }, _ -> unify t1' t2
  | _, Type.Var { contents = Some t2' } -> unify t1  t2'
  | Type.Fun (t1s, t1'), Type.Fun (t2s, t2') ->
    (try List.iter2 unify t1s t2s
     with Invalid_argument _ ->
       (* 関数の引数の数がt1とt2で異なる *)
       raise (Unify (t1, t2)));
    unify t1' t2'
  | Type.Tuple t1s, Type.Tuple t2s ->
    (try List.iter2 unify t1s t2s
     with Invalid_argument _ ->
       (* タプルの要素の個数がt1とt2で異なる *)
       raise (Unify (t1, t2)))
  | Type.Array t1, Type.Array t2 -> unify t1 t2
  (* それ以外の場合: t1==t2とすると矛盾が生じる *)
  | _, _ -> raise (Unify(t1, t2))

(* 環境envにおいて式eの持つ型は何か? *)
(* 推論の中で確定する型変数を(暗黙に)置き換える *)
let rec g env e = (* 型推論ルーチン (caml2html: typing_g) *)
  try
    match e with
    (* ベースケース *)
    | Unit       -> Type.Unit
    | Bool  (_)  -> Type.Bool
    | Int   (_)  -> Type.Int
    | Float (_)  -> Type.Float
    | Tuple (es) -> Type.Tuple(List.map (g env) es)
    (* - 変数: 対応する型がenvにあるならその型を返す、無いなら型を新しく割り振ってそれを返す *)
    | Var(x) when M.mem x env     -> M.find x env (* 変数の型推論 (caml2html: typing_var) *)
    | Var(x) when M.mem x !extenv -> M.find x !extenv
    | Var(x) -> (* 外部変数の型推論 (caml2html: typing_extvar) *)
      Format.eprintf "free variable %s assumed as external@." x;
      let t = Type.gentyp () in
      extenv := M.add x t !extenv; (* 外部変数の辞書を更新 *)
      t
    (* 再帰ステップ *)
    (* - 型チェックと環境の更新が行われる式 *) (* letの型推論 (caml2html: typing_let) *)
    | Let      ((x, t), e1, e2) ->
      unify t (g env e1);
      g (M.add x t env) e2
    | LetTuple (   xts, e1, e2) ->
      unify (Type.Tuple(List.map snd xts)) (g env e1);
      g (M.add_list xts env) e2
    | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recの型推論 (caml2html: typing_letrec) *)
      let env = M.add x t env in
      unify t (Type.Fun(List.map snd yts, g (M.add_list yts env) e1));
      g env e2
    (* - 型チェックのみ行われる式 *)
    | FAdd (e1, e2)
    | FSub (e1, e2)
    | FMul (e1, e2)
    | FDiv (e1, e2)  -> unify Type.Float (g env e1); unify Type.Float (g env e2); Type.Float
    | Add  (e1, e2)     (* 足し算（と引き算）の型推論 (caml2html: typing_add) *)
    | Sub  (e1, e2)  -> unify Type.Int   (g env e1); unify Type.Int   (g env e2); Type.Int
    | Eq   (e1, e2)
    | LE   (e1, e2)  -> unify (g env e1) (g env e2); Type.Bool (* ad-hoc polymorphism: 型が同じことだけ確認 *)
    | Not  (e)       -> unify Type.Bool  (g env e ); Type.Bool
    | Neg  (e)       -> unify Type.Int   (g env e ); Type.Int
    | FNeg (e)       -> unify Type.Float (g env e ); Type.Float
    | If(e1, e2, e3) ->
      unify (g env e1) Type.Bool;
      let t2 = g env e2 in
      let t3 = g env e3 in
      unify t2 t3;
      t2
    | App(e, es) -> (* 関数適用の型推論 (caml2html: typing_app) *)
      let t = Type.gentyp () in
      unify (g env e) (Type.Fun(List.map (g env) es, t));
      t
    | Array(e1, e2) -> (* must be a primitive for "polymorphic" typing *)
      unify (g env e1) Type.Int;
      Type.Array(g env e2)
    | Get(e1, e2) ->
      let t = Type.gentyp () in
      unify (Type.Array(t)) (g env e1);
      unify Type.Int (g env e2);
      t
    | Put(e1, e2, e3) ->
      let t = g env e3 in
      unify (Type.Array(t)) (g env e1);
      unify Type.Int (g env e2);
      Type.Unit
  (* その他: エラー *)
  with Unify(t1, t2) ->
    (* エラーを出力 *)
    Error.explain_type_error (deref_term e, t1, t2)

let f e =
  extenv := M.empty;
  (try unify Type.Unit (g M.empty e) (* トップレベルはUnit型を持つという仮定のもと型変数の値を決定する *)
   with Unify _ ->                   (* トップレベルに限っては矛盾してもOK *)
     Format.eprintf "warning: final result does not have type unit@.");
  extenv := M.map deref_typ !extenv; (* 外部変数に含まれる型変数を取り除く *)
  deref_term e                       (* トップレベルの式から型変数を取り除く *)
