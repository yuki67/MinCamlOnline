open FormatUtil

(* 関数定義: 引数だけでなく自由変数も保持する *)
type fundef = {
  name : Id.l * Type.t;             (* 関数の名前 *)
  args : (Id.t * Type.t) list;      (* 関数の引数 *)
  formal_fv : (Id.t * Type.t) list; (* 関数の自由変数 *)
  body : t                          (* 関数の本体 *)
}

(* クロージャは関数のラベルに情報を足したものである
 * 具体的には、ラベルが指す関数が持つ自由変数と変数の対応関係を保持する *)
and closure = {
  entry : Id.l;         (* 関数定義を指す変数 *)
  actual_fv : Id.t list (* entryが指す関数を呼び出すときに、自由変数に代入されるべき値 *)
}

and t =
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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  (* let rec f x = e1 in e2
   * -> MakeCls ((f, [fの型]),
   *             {entry=Id.L(x), actual_fv=[e1に含まれる自由変数の、現時点での値(を指す変数)]},
   *             e2') *)
  | MakeCls of (Id.t * Type.t) * closure * t
  (* クロージャ関数の適用 *)
  (* クロージャを指している変数が第一引数なので、型はId.t *)
  | AppCls of Id.t * Id.t list
  (* クロージャを使わない関数の適用 *)
  (* 第一引数はトップレベルの関数を指すので、型はId.L *)
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l

(* プログラム全体は関数定義とトップレベルの式からなる *)
and prog = Prog of fundef list * t

(* 式に含まれる自由変数、つまり式だけを見てもどの値を指しているのかわからない変数のリストを返す
 * fv "let x = 1 + y in x + z" -> [y; z] *)
let rec fv = function
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) ->
    (* 定義されるのが再帰関数の場合lが指す関数の本体にxが含まれるので、"S.remove x"が必要 *)
    S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) | Tuple(xs) -> S.of_list xs
  | LetTuple(xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | Put(x, y, z) -> S.of_list [x; y; z]

(* プログラム全体で定義される関数のリスト *)
(* 自由変数の有無にかかわらず全てここに入れられる *)
let toplevel : fundef list ref = ref []

(* KNormal.t型の式をClosure.t型に変換する
   env   : これまでに定義された変数からその型への写像
   known : f ∈ known <-> 関数fの本体には自由変数がないので、呼び出しにクロージャが使われない *)
let rec g env known = function (* クロージャ変換ルーチン本体 (caml2html: closure_g) *)
  (* ベースケース *)
  | KNormal.Unit -> Unit
  | KNormal.Int(i) -> Int(i)
  | KNormal.Float(d) -> Float(d)
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.Var(x) -> Var(x)
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(Id.L(x))
  | KNormal.ExtFunApp(x, ys) -> AppDir(Id.L("min_caml_" ^ x), ys)
  (* - S.mem f known <-> トップレベルに関数fの定義が登録されており、かつクロージャ無しで呼べる *)
  | KNormal.App(f, ys) when S.mem f known -> (* 関数適用の場合 (caml2html: closure_app) *)
    Format.eprintf "directly applying %s@." f;
    AppDir(Id.L(f), ys)
  (* - Appの第一引数がknownに無い場合、xはクロージャを指す *)
  | KNormal.App(x, ys) -> AppCls(x, ys)

  (* 再帰ステップ *)
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, g env known e1, g env known e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (M.add x t env) known e2)
  | KNormal.LetTuple(xts, y, e) -> LetTuple(xts, y, g (M.add_list xts env) known e)

  (* - まずe1の自由変数を求め、関数をtoplevelに登録する
   * -- 自由変数がない場合はe2だけを残す
   * -- 自由変数がある場合はクロージャを生成してからe2を実行するコードにする *)
  | KNormal.LetRec
      ({ KNormal.name = (x, t);
         KNormal.args = yts;
         KNormal.body = e1 },
       e2) -> (* 関数定義の場合 (caml2html: closure_letrec) *)
    (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
       xに自由変数がない(closureを介さずdirectに呼び出せる)
       と仮定し、knownに追加してe1をクロージャ変換してみる *)
    let toplevel_backup = !toplevel in
    let env' = M.add x t env in
    let known' = S.add x known in
    let e1' = g (M.add_list yts env') known' e1 in
    (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
    (* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
       (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) *)
    let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
    let known', e1' =
      if S.is_empty zs
      then known', e1' (* e1'に関数の引数以外の自由変数がない -> クロージャはいらない *)
      else             (* e1'に関数の引数以外の自由変数が出てくる -> クロージャを作る必要がある *)
        (* 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す *)
        (Format.eprintf "free variable(s) %s found in function %s@." (Id.pp_list (S.elements zs)) x;
         Format.eprintf "function %s cannot be directly applied in fact@." x;
         toplevel := toplevel_backup;
         let e1' = g (M.add_list yts env') known e1 in
         known, e1') in
    let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in (* 自由変数のリスト *)
    let zts = List.map (fun z -> (z, M.find z env')) zs in (* ここで自由変数zの型を引くために引数envが必要 *)
    toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* トップレベル関数を追加 *)
    let e2' = g env' known' e2 in
    if S.mem x (fv e2') then (* xが変数としてe2'に出現するか *)
      MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') (* 出現していたら削除しない *)
    else
      (* xがAppDirでだけ使われるとき(xの本体に出現する自由変数がExtArrayのときなど)はセーフ *)
      (Format.eprintf "eliminating closure(s) %s@." x;
       e2') (* 出現しなければMakeClsを削除 *)

let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog(List.rev !toplevel, e')

let rec format_string (Prog (defs, knorm)) =
  binary "Prog"
    (format_string_of_list defs format_string_of_def)
    (format_string_of_knorm knorm)

and format_string_of_def {name=(Id.L name, _); args=args; formal_fv=fvs; body=body} =
  Printf.sprintf "(@[<hv 0>%s,@ %s,@ %s,@ %s)@]"
    (quoted name)
    (format_string_of_list (List.map fst args) quoted)
    (format_string_of_list (List.map fst fvs) quoted)
    (format_string_of_knorm body)

and format_string_of_knorm = function
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
      (format_string_of_knorm k1) (format_string_of_knorm k2)
  | IfLE (id1, id2, k1, k2) ->
    quaternary "IfLE"
      (unary "Var" id1) (unary "Var" id2)
      (format_string_of_knorm k1) (format_string_of_knorm k2)
  | Let ((id, t), k1, k2) ->
    Printf.sprintf "@[<v 0>Let (@[<0>(%s, %s),@ %s,@]@ %s)@]"
      (quoted id)
      (Type.format_string t)
      (format_string_of_knorm k1)
      (format_string_of_knorm k2)
  | Var id -> unary "Var" id;
  | AppCls (id, idlist) ->
    binary "AppCls" (unary "Var" id) (format_string_of_list idlist (unary "Var"))
  | Tuple idlist -> unary "Tuple" (format_string_of_list idlist (unary "Var"))
  | LetTuple (alist, id, k) ->
    Printf.sprintf "@[<v 0>LetTuple (@[<0>%s,@ %s,@]@ %s)@]"
      (format_string_of_list (List.map fst alist) quoted)
      (unary "Var" id)
      (format_string_of_knorm k)
  | Get (id1, id2) -> binary "Get" (unary "Var" id1) (unary "Var" id2)
  | Put (id1, id2, id3) ->
    ternary "Put" (unary "Var" id1) (unary "Var" id2) (unary "Vare" id3)
  | ExtArray (Id.L id) -> unary "ExtArray" (unary "Var" id)
  | AppDir (Id.L id, idlist) ->
    binary "AppDir" (unary "Var" id) (format_string_of_list idlist (unary "Var"))
  | MakeCls ((id, _), {entry=Id.L entry; actual_fv=afvs}, knorm) ->
    Printf.sprintf "@[<v 0>MakeCls (@[<0>%s,@ (%s,@ %s)@]@ %s)@]"
      (quoted id)
      (quoted entry)
      (format_string_of_list afvs quoted)
      (format_string_of_knorm knorm)

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
