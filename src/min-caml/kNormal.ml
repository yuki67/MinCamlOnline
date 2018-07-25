open FormatUtil

(* このファイルのコメントはYu-go193によって付けられた *)

(* give names to intermediate values (K-normalization) *)

type t = (* K正規化後の式 (caml2html: knormal_t) *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 (caml2html: knormal_branch) *)(*if(x == y) then e1 else e2に対応*)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t(*let (変数名,型) = e1 in e2に対応*)
  | Var of Id.t(*変数名*)
  | LetRec of fundef * t(*LetRec(name:(関数名,型);args:(変数名:型)のリスト;body:式e1) in e2に対応*)
  | App of Id.t * Id.t list(*(関数名f, 変数名[x1...xn])に対応*)
  | Tuple of Id.t list(*(x1,x2,...,xn)、変数名のリストに対応、順序は*)
  | LetTuple of (Id.t * Type.t) list * Id.t * t(*Let(x1,...,xn)=y in eに対応*)
  | Get of Id.t * Id.t(*Get(x,y)が配列の要素の読み込みx.(y)に対応*)
  | Put of Id.t * Id.t * Id.t(*Put(x,y,z)が配列の要素への書き込みx.(y)<-zに対応*)
  | ExtArray of Id.t(**)
  | ExtFunApp of Id.t * Id.t list(*外部関数へのApp*)
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec fv = function (* 式に出現する（自由な）変数 (caml2html: knormal_fv) *)
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y](*式中の自由変数はx,y*)
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))(*式中の自由変数はx,y,e1の自由変数,e2の自由変数*)
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))(*式中の自由変数はe1の自由変数+(e2の自由変数-x)*)
  | Var(x) -> S.singleton x(*式中の自由変数はx*)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
    let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in(*zsはe1の自由変数から引数を除いたもの、引数は自由変数ではない*)
    S.diff (S.union zs (fv e2)) (S.singleton x)(*式中の自由変数はzsとe2の自由変数から関数名xを除いたもの*)
  | App(x, ys) -> S.of_list (x :: ys)(*式中の自由変数はxとysの中身*)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs(*式中の自由変数はxsの中身*)
  | Put(x, y, z) -> S.of_list [x; y; z](*式中の自由変数はx,y,z*)
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))(*式中の自由変数はyと式eの自由変数から(x1,...,xn)を除いたもの*)

let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
    let x = Id.gentmp t in
    let e', t' = k x in
    Let((x, t), e, e'), t'

let rec g env = function (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  | Syntax.Unit -> Unit, Type.Unit
  | Syntax.Bool(b) -> Int(if b then 1 else 0), Type.Int (* 論理値true, falseを整数1, 0に変換 (caml2html: knormal_bool) *)
  | Syntax.Int(i) -> Int(i), Type.Int
  | Syntax.Float(d) -> Float(d), Type.Float
  | Syntax.Not(e) -> g env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
  | Syntax.Neg(e) ->
    insert_let (g env e)
      (fun x -> Neg(x), Type.Int)
  | Syntax.Add(e1, e2) -> (* 足し算のK正規化 (caml2html: knormal_add) *)(*基本的にそれぞれの式を再帰的にK正規化*)
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> Add(x, y), Type.Int))
  | Syntax.Sub(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> Sub(x, y), Type.Int))
  | Syntax.FNeg(e) ->
    insert_let (g env e)
      (fun x -> FNeg(x), Type.Float)
  | Syntax.FAdd(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> FAdd(x, y), Type.Float))
  | Syntax.FSub(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> FSub(x, y), Type.Float))
  | Syntax.FMul(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> FMul(x, y), Type.Float))
  | Syntax.FDiv(e1, e2) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> FDiv(x, y), Type.Float))
  | Syntax.Eq _ | Syntax.LE _ as cmp ->
    g env (Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
  | Syntax.If(Syntax.Not(e1), e2, e3) -> g env (Syntax.If(e1, e3, e2)) (* notによる分岐を変換 (caml2html: knormal_not) *)
  | Syntax.If(Syntax.Eq(e1, e2), e3, e4) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y ->
             let e3', t3 = g env e3 in
             let e4', t4 = g env e4 in
             IfEq(x, y, e3', e4'), t3))(*e1,e2,e3,e4を再帰的にK正規化、e3とe4の型は同じことに注意*)
  | Syntax.If(Syntax.LE(e1, e2), e3, e4) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y ->
             let e3', t3 = g env e3 in
             let e4', t4 = g env e4 in
             IfLE(x, y, e3', e4'), t3))
  | Syntax.If(e1, e2, e3) -> g env (Syntax.If(Syntax.Eq(e1, Syntax.Bool(false)), e3, e2)) (* 比較のない分岐を変換 (caml2html: knormal_if) *)
  | Syntax.Let((x, t), e1, e2) ->
    let e1', t1 = g env e1 in
    let e2', t2 = g (M.add x t env) e2 in
    Let((x, t), e1', e2'), t2 (*e1とe2を再帰的にK正規化、e2をK正規化する際は(x,t)を環境に追加(自由変数ではない！)*)
  | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env (*何もしない*)
  | Syntax.Var(x) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
    (match M.find x !Typing.extenv with
     | Type.Array(_) as t -> ExtArray x, t(*外部配列ExtArrayへの変換はここで行う*)
     | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
    let env' = M.add x t env in
    let e2', t2 = g env' e2 in(*関数定義後の式のK正規化では関数を環境に入れる必要がある*)
    let e1', t1 = g (M.add_list yts env') e1 in(*関数定義の式のK正規化では関数自身と引数を環境に入れる必要がある*)
    LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
  | Syntax.App(Syntax.Var(f), e2s) when not (M.mem f env) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
    (match M.find f !Typing.extenv with
     | Type.Fun(_, t) ->
       let rec bind xs = function (* "xs" are identifiers for the arguments *)
         | [] -> ExtFunApp(f, xs), t
         | e2 :: e2s ->
           insert_let (g env e2)
             (fun x -> bind (xs @ [x]) e2s) in
       bind [] e2s (* left-to-right evaluation *)
     | _ -> assert false)
  | Syntax.App(e1, e2s) ->
    (match g env e1 with
     | _, Type.Fun(_, t) as g_e1 ->
       insert_let g_e1
         (fun f ->
            let rec bind xs = function (* "xs" are identifiers for the arguments *)
              | [] -> App(f, xs), t
              | e2 :: e2s ->
                insert_let (g env e2)
                  (fun x -> bind (xs @ [x]) e2s) in
            bind [] e2s) (* left-to-right evaluation *)(*e1をK正規化後、引数のリストであるe2sをそれぞれK正規化*)
     (*ここでf(x1,...,xn)を表す引数リストが[xn,...,x1]から[x1,...,xn]に変わる*)
     | _ -> assert false)
  | Syntax.Tuple(es) ->
    let rec bind xs ts = function (* "xs" and "ts" are identifiers and types for the elements *)
      | [] -> Tuple(xs), Type.Tuple(ts)
      | e :: es ->
        let _, t as g_e = g env e in
        insert_let g_e
          (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
    bind [] [] es (*Tupleの各要素についてK正規化、このとき(x1,...xn)に対して順序を[xn,...,x1]から[x1,...,xn]にする*)
  | Syntax.LetTuple(xts, e1, e2) ->
    insert_let (g env e1)
      (fun y ->
         let e2', t2 = g (M.add_list xts env) e2 in
         LetTuple(xts, y, e2'), t2)
  | Syntax.Array(e1, e2) ->
    insert_let (g env e1)
      (fun x ->
         let _, t2 as g_e2 = g env e2 in
         insert_let g_e2
           (fun y ->
              let l =
                match t2 with
                | Type.Float -> "create_float_array"
                | _ -> "create_array" in
              ExtFunApp(l, [x; y]), Type.Array(t2)))
  (*まず式e1とe2をそれぞれK正規化
    Arrayの作成は、外部関数create_float_array n xまたはcreate_array n xの呼び出し*)
  | Syntax.Get(e1, e2) ->
    (match g env e1 with
     |        _, Type.Array(t) as g_e1 -> (*返り値の型を得る*)
       insert_let g_e1
         (fun x -> insert_let (g env e2)
             (fun y -> Get(x, y), t))(*e1,e2をK正規化*)
     | _ -> assert false)
  | Syntax.Put(e1, e2, e3) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
          (fun y -> insert_let (g env e3)
              (fun z -> Put(x, y, z), Type.Unit)))(*e1,e2,e3をK正規化*)

let f e = fst (g M.empty e)

let rec format_string = function
  | Unit -> "@[<1>()@]"
  | Int i -> Printf.sprintf "@[<1>Int %d@]" i
  | Float f -> Printf.sprintf "@[<1>Float %f@]" f
  | Neg id -> unary "Neg" (unary "Var" id)
  | Add (id1, id2) -> binary "Add" (unary "Var" id1) (unary "Var" id2)
  | Sub (id1, id2) -> binary "Sub" (unary "Var" id1) (unary "Var" id2)
  | FNeg id -> unary "FNeg" (unary "Var" id)
  | FAdd (id1, id2) -> binary "FAdd" (unary "Var" id1) (unary "Var" id2)
  | FSub (id1, id2) -> binary "FSub" (unary "Var" id1) (unary "Var" id2)
  | FMul (id1, id2) -> binary "FMul" (unary "Var" id1) (unary "Var" id2)
  | FDiv (id1, id2) -> binary "FDiv" (unary "Var" id1) (unary "Var" id2)
  | IfEq (id1, id2, k1, k2) ->
    quaternary "IfEq"
      (unary "Var" id1) (unary "Var" id2)
      (format_string k1) (format_string k2)
  | IfLE (id1, id2, k1, k2) ->
    quaternary "IfLE"
      (unary "Var" id1) (unary "Var" id2)
      (format_string k1) (format_string k2)
  | Let ((id, _), k1, k2) ->
    Printf.sprintf "@[<v 0>Let (@[<0>%s,@ %s,@]@ %s)@]"
      (quoted id)
      (format_string k1)
      (format_string k2)
  | Var id -> unary "Var" id;
  | LetRec (def, k) ->
    Printf.sprintf "@[<v 0>LetRec (@[<0>%s,@ %s,@ %s,@]@ %s)@]"
      (quoted (fst def.name))
      (format_string_of_list (List.map fst def.args) quoted)
      (format_string def.body)
      (format_string k)
  | App (id, idlist) ->
    binary "App" (unary "Var" id) (format_string_of_list idlist (unary "Var"))
  | Tuple idlist -> unary "Tuple" (format_string_of_list idlist (unary "Var"))
  | LetTuple (alist, id, k) ->
    Printf.sprintf "@[<v 0>LetTuple (@[<0>%s,@ %s,@]@ %s)@]"
      (format_string_of_list (List.map fst alist) quoted)
      (unary "Var" id)
      (format_string k)
  | Get (id1, id2) -> binary "Get" (unary "Var" id1) (unary "Var" id2)
  | Put (id1, id2, id3) ->
    ternary "Put" (unary "Var" id1) (unary "Var" id2) (unary "Var" id3)
  | ExtArray id -> unary "ExtArray" (unary "Var" id)
  | ExtFunApp (id, idlist) ->
    binary "ExtFunApp" (unary "Var" id) (format_string_of_list idlist (unary "Var"))

let print fmt k =
  k
  |> format_string
  |> (fun str -> Scanf.format_from_string str "")
  |> Format.fprintf fmt

let string k =
  k
  |> format_string
  |> (fun str -> Scanf.format_from_string str "")
  |> Format.sprintf
