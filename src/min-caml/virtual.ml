(* translation into assembly with infinite number of virtual registers *)
(* asm.mlのコメントも参照 *)

(*M.xxxはほぼMap.Make.xxx。詳細はm.ml*)
(*環境envは変数名(またはラベル名)から型への写像*)
open Asm
 (*
 type t = (* 命令の列 (caml2html: sparcasm_t) *)
   | Ans of exp
   | Let of (Id.t * Type.t) * exp * t
 and exp = (* 一つ一つの命令に対応する式 (caml2html: sparcasm_exp) *)
   | Nop
   | Set of int
   (*中略*)
   | Restore of Id.t (* スタック変数から値を復元 (caml2html: sparcasm_restore) *)
 *)

let data = ref [] (* 浮動小数点数の定数テーブル (caml2html: virtual_data) *)

let classify xts ini addf addi =
  List.fold_left
    (fun acc (x, t) -> (*xは変数名、tはその型*)
       match t with
       | Type.Unit -> acc (*変数の型がunitの時は、何も追加しない*)
       | Type.Float -> addf acc x (*型がfloatの時、追加する。(何を何に追加するのかは呼び出し元によって異なる)*)
       | _ -> addi acc x t) (*型がintの時、追加する。*)
    ini
    xts

let separate xts = (*変数を、型がintのものとfloatのものに分ける*)
  classify
    xts
    ([], [])
    (fun (int, float) x -> (int, float @ [x]))(*変数の値の型がfloatの時、floatの変数のリストに追加*)
    (fun (int, float) x _ -> (int @ [x], float))(*変数の値の型がintの時、intの変数のリストに追加*)

(*iniは初期の(オフセット,命令列)。
  xts内の各(x,t)について、命令列の先頭にストアまたはロード命令を追加していくことを繰り返すためのルーチン*)
let expand xts ini addf addi =
  classify
    (*xts内の各(x,t)に対して繰り返す*)
    xts
    ini
    (fun (offset, acc) x ->
       let offset = align offset in (*let align i = (if i mod 8 = 0 then i else i + 4)*)
       (offset + 8, addf x offset acc)) (*t(xの型)がfloatの時は、オフセットを8増やし、関数addfを使う*)
    (fun (offset, acc) x t ->
       (offset + 4, addi x t offset acc)) (*t(xの型)がfloat以外の時は、オフセットを4増やし、関数addiを使う*)

let rec g env = function (* 式の仮想マシンコード生成 (caml2html: virtual_g) *)
  (*ベースケース*)
  | Closure.Unit -> Ans(Nop)
  | Closure.Int(i) -> Ans(Set(i)) (*emit.mlでmovl命令に変換される*)
  | Closure.Float(d) ->
    let l =
      try
        (* すでに定数テーブルにあったら再利用 *)
        let (l, _) = List.find (fun (_, d') -> d = d') !data in
        l
      with Not_found ->
        let l = Id.L(Id.genid "l") in (*新しいラベルを作る*)
        data := (l, d) :: !data; (*新しいラベルと浮動小数点数の対応を定数テーブルに追加する*)
        l in
    let x = Id.genid "l" in (*変数(レジスタ)を用意*)
    (*  | NonTail(x), SetL(Id.L(y)) -> Printf.fprintf oc "\tmovl\t$%s, %s\n" y x *)
    (*  | NonTail(x), LdDF(y, C(j), i) -> Printf.fprintf oc "\tmovsd\t%d(%s), %s\n" (j * i) y x *)
    (*変数(レジスタ)にラベルを入れた後、ロード命令をする。これで定数テーブル上の浮動小数点数を引き出せる*)
    Let((x, Type.Int), SetL(l), Ans(LdDF(x, C(0), 1)))
  | Closure.Neg(x) -> Ans(Neg(x))
  (*type id_or_imm = V of Id.t | C of int*)
  (*Vの中身は(即値ではなく)変数名*)
  (*Cの中身は即値として扱われる定数*)
  | Closure.Add(x, y) -> Ans(Add(x, V(y)))
  | Closure.Sub(x, y) -> Ans(Sub(x, V(y)))
  | Closure.FNeg(x) -> Ans(FNegD(x))
  | Closure.FAdd(x, y) -> Ans(FAddD(x, y))
  | Closure.FSub(x, y) -> Ans(FSubD(x, y))
  | Closure.FMul(x, y) -> Ans(FMulD(x, y))
  | Closure.FDiv(x, y) -> Ans(FDivD(x, y))
  | Closure.Var(x) ->
    (match M.find x env with
     | Type.Unit -> Ans(Nop)
     | Type.Float -> Ans(FMovD(x))(*movsd命令*)
     | _ -> Ans(Mov(x)))(*movl命令*)
  | Closure.AppCls(x, ys) ->
    let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in (*引数を型で分類*)
    Ans(CallCls(x, int, float))(*クロージャによる呼び出し*)
  | Closure.AppDir(Id.L(x), ys) ->
    let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
    Ans(CallDir(Id.L(x), int, float))(*ラベルを使って直接、関数呼び出し*)
  | Closure.Tuple(xs) -> (* 組の生成 (caml2html: virtual_tuple) *)
    let y = Id.genid "t" in
    let (offset, store) =
      expand
        (List.map (fun x -> (x, M.find x env)) xs) (*組の要素の(名前,型)のリスト*)
        (0, Ans(Mov(y)))(*初期の(オフセット,組生成後の式(ヒープ中の組の先頭のアドレスが入ったレジスタからのmov命令))*)
        (*let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)*)
        (* seqは shorthand of Let for unit *)
        (*命令列storeを受け取り、組の要素の型がfloatの時は先頭にmovsd命令を付け加えた命令列を返す。
          float以外の時は先頭にmovl命令を付け加えた命令列を返す。
          これを繰り返すことで、「組の要素をストアした後、組の先頭のアドレスを返す」命令列ができる。*)
        (fun x offset store -> seq(StDF(x, y, C(offset), 1), store))
        (fun x _ offset store -> seq(St(x, y, C(offset), 1), store)) in
    (*reg_hpはasm.mlで定義。ヒープポインタ。*)
    (*以下は、組を生成して組の先頭のアドレスを返す命令列。*)
    Let((y, Type.Tuple(List.map (fun x -> M.find x env) xs)), Mov(reg_hp), (*現在のヒープポインタの値を組の先頭のアドレスとする*)
        Let((reg_hp, Type.Int), Add(reg_hp, C(align offset)), (*ヒープポインタの値を組のサイズ分だけ増やす*)
            store)) (*組の要素をストアした後、組の先頭のアドレスを返す*)
  (*組の先頭のアドレスを組そのものの値としている。組の読み出しはこのアドレスを介して行う。*)
  | Closure.Get(x, y) -> (* 配列の読み出し (caml2html: virtual_get) *)(* x[y] を取り出す。*)
    (match M.find x env with
     | Type.Array(Type.Unit) -> Ans(Nop)(*xの型がunitの配列→ロードしない*)
     | Type.Array(Type.Float) -> Ans(LdDF(x, V(y), 8)) (*アドレスx+8*yからロード*)
     | Type.Array(_) -> Ans(Ld(x, V(y), 4))(*アドレスx+4*yからロード*)
     | _ -> assert false)(*型が配列でないxに対して配列の読み出しを行った場合*)
  | Closure.Put(x, y, z) -> (*配列への書き込み*)
    (match M.find x env with
     | Type.Array(Type.Unit) -> Ans(Nop) (*xの型がunitの配列→書き込まない*)
     | Type.Array(Type.Float) -> Ans(StDF(z, x, V(y), 8)) (*アドレスx+8*yにzを書き込む*)
     | Type.Array(_) -> Ans(St(z, x, V(y), 4)) (*アドレスx+4*yにzを書き込む*)
     | _ -> assert false) (*型が配列でないxに対して配列への書き込みを行った場合*)
  | Closure.ExtArray(Id.L(x)) -> Ans(SetL(Id.L("min_caml_" ^ x))) (*外部配列はラベルを介して参照する*)

  (*再帰ステップ*)
  | Closure.IfEq(x, y, e1, e2) ->
    (*if x=y then e1 else e2*)
    (match M.find x env with
     | Type.Bool | Type.Int -> Ans(IfEq(x, V(y), g env e1, g env e2)) (*if文中の式e1,e2を変換(再帰)*)
     | Type.Float -> Ans(IfFEq(x, y, g env e1, g env e2))
     | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE(x, y, e1, e2) ->
    (*if x<=y then e1 else e2*)
    (match M.find x env with
     | Type.Bool | Type.Int -> Ans(IfLE(x, V(y), g env e1, g env e2))
     | Type.Float -> Ans(IfFLE(x, y, g env e1, g env e2))
     | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.Let((x, t1), e1, e2) ->
    let e1' = g env e1 in
    let e2' = g (M.add x t1 env) e2 in
    concat e1' (x, t1) e2'
  (*concatはasm.mlで定義。*)
  (*仮想マシンコードの命令列2つをLetで連結する*)
  (* 意味としては、e1'が
     let p = ... in
     let q = ... in
     r
     を表すコードで、e2'が
     let s = ... in
     let t = ... in
     u
     を表すコードのとき
     concat e1' (x,t) e2 は
     let p = ... in
     let q = ... in
     let x = r in (*型はt*)
     let s = ... in
     let u = ... in
     v
     を表すコードを返すということである。
  *)
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) -> (* クロージャの生成 (caml2html: virtual_makecls) *)
    (* Closureのアドレスをセットしてから、自由変数の値をストア *)
    let e2' = g (M.add x t env) e2 in (*クロージャの変数名と型の対応を加えた型環境で、クロージャの定義後の式のコードを生成*)
    let offset, store_fv =
      expand
        (List.map (fun y -> (y, M.find y env)) ys) (*自由変数の値の(名前,型)のリスト*)
        (4, e2') (*初期の(オフセット,クロージャ生成後の式)*)
        (*let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)*)
        (* seqは shorthand of Let for unit *)
        (*命令列store_fvを受け取り、自由変数の値の型がfloatの時は先頭にmovsd命令を付け加えた命令列を返す。
          float以外の時は先頭にmovl命令を付け加えた命令列を返す。
          これを繰り返すことで、「自由変数の値をストアした後、クロージャ生成後の式を実行する」命令列ができる。*)
        (fun y offset store_fv -> seq(StDF(y, x, C(offset), 1), store_fv))
        (fun y _ offset store_fv -> seq(St(y, x, C(offset), 1), store_fv)) in
    Let((x, t), Mov(reg_hp), (*reg_hpはasm.mlで定義。ヒープポインタ。*)
        Let((reg_hp, Type.Int), Add(reg_hp, C(align offset)),
            let z = Id.genid "l" in
            Let((z, Type.Int), SetL(l),
                seq(St(z, x, C(0), 1),
                    store_fv))))
  (*以上の処理で、「オフセット分だけヒープポインタの値を増やして、(元のヒープポインタの値を使って)ヒープに関数本体のラベルと自由変数の値をストアする」というコードを生成している。
    元のヒープポインタの値は変数(レジスタ)xに格納され、これを介してクロージャの適用を行う。*)
  | Closure.LetTuple(xts, y, e2) -> (*組の読み出し*)
    let s = Closure.fv e2 in (*sはe2内に出現する自由変数の集合*)
    let (offset, load) =
      expand
        xts
        (*let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys *)
        (0, g (M.add_list xts env) e2) (*xts((変数名,型)のリスト)を加えた環境で、e2の仮想マシンコードを生成*)
        (fun x offset load -> (*xの型がfloatの場合*)
           if not (S.mem x s) then load (*e2中に出現しない変数に対しては、そのままの命令列を返す。(組から値を呼び出さない)*) else (* [XX] a little ad hoc optimization *)
             (*let fletd(x, e1, e2) = Let((x, Type.Float), e1, e2)*)
             (* shorthand of Let for unit *)
             fletd(x, LdDF(y, C(offset), 1), load)) (*e2中に出現する変数に対しては、組から値を読み出す命令を先頭に追加した命令列を返す。*)
        (fun x t offset load -> (*xの型がfloat以外の場合*)
           if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
             Let((x, t), Ld(y, C(offset), 1), load)) in
    (*xtsのすべてのx(変数名)に対してこれを繰り返すことで、「必要な要素を組から読み出した後にe2を実行する」命令列ができる*)
    load

(* 関数の仮想マシンコード生成 (caml2html: virtual_h) *)
let h { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } = (*formal_fvは自由変数*)
  let (int, float) = separate yts in (*引数の変数を、型がintのものとfloatのものに分ける*)
  let (offset, load) =
    expand
      zts (*自由変数リストzts内の各(z,zt)に対して繰り返す*)
      (*let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys*)
      (4, g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e) (*自由変数と引数の変数と関数のラベルを入れた環境(名前→型)で、関数の本体の式を仮想マシンコードに変換したものが初期の命令列である。初期のoffsetは4とする。*)
      (*let fletd(x, e1, e2) = Let((x, Type.Float), e1, e2) *)
      (fun z offset load -> fletd(z, LdDF(x, C(offset), 1), load)) (*命令列の先頭に、クロージャから自由変数の値をロードする命令を追加する*)
      (fun z t offset load -> Let((z, t), Ld(x, C(offset), 1), load)) in
  (*(x86向けでは)クロージャによる関数呼び出しを行う際は、そのクロージャのアドレスを、関数のラベル名に対応するレジスタで与えるようだ*)
  (*以上の処理で、クロージャから自由変数の値をロードしてから関数本体を実行する命令列ができる*)
  match t with
  | Type.Fun(_, t2) ->
    { name = Id.L(x); args = int; fargs = float; body = load; ret = t2 }
  | _ -> assert false

(* プログラム全体の仮想マシンコード生成 (caml2html: virtual_f) *)
let f (Closure.Prog(fundefs, e)) = (*(ブログラム全体)=(トップレベル関数のリスト,メインルーチンの式)*)
  data := []; (*dataは浮動小数点数の定数テーブル*)
  let fundefs = List.map h fundefs in (*各トップレベル関数を仮想マシンコードに変換*)
  let e = g M.empty e in (*メインルーチンの式を仮想マシンコードに変換*)
  Prog(!data, fundefs, e) (*(プログラム全体)=(浮動小数点数の定数テーブル,トップレベル関数のリスト,メインルーチンの式)*)
