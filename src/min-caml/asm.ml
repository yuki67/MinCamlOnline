open FormatUtil

(* 2オペランドではなく3オペランドのx86アセンブリもどき *)

type id_or_imm =
  | V of Id.t (* 変数(variable) *)
  | C of int  (* 定数/即値 (constant) *)
type t = (* 命令の列 (caml2html: sparcasm_t) *)
  | Ans of exp                       (* "e の返り値を返り値レジスタに入れてreturn"を表す *)
  (* "e1の返り値をレジスタxに入れ、続けてe2を実行"を表す *)
  (* xがレジスタを表さないときもある("Id.gentmp typ" など) *)
  | Let of (Id.t * Type.t) * exp * t
and exp = (* 一つ一つの命令に対応する式 (caml2html: sparcasm_exp) *)
  (* 命令の実行結果をどのレジスタに格納するかという情報をこの型は保持しないことに注意 *)
  (* (保持する必要があるなら)Asm.t型のLetが保持する *)
  | Nop
  | Set of int   (* 即値(int)の設定 *)
  | SetL of Id.l (* 即値(ラベル)の設定 *)
  | Mov of Id.t  (* レジスタ間の値のコピー *)
  | Neg of Id.t  (* マイナス (論理否定でない) *)
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  (* Ld/St (x, V(y), d) : メモリのx+d*y番地{から読み込み / へ保存} *)
  (* Ld/St (x, C(y), d) : メモリのx+y+d番地{から読み込み / へ保存} *)
  | Ld of Id.t * id_or_imm * int
  | St of Id.t * Id.t * id_or_imm * int
  | FMovD of Id.t (* float値のレジスタ間コピー(Dは"Double") *)
  | FNegD of Id.t (* マイナス (論理否定でない) *)
  | FAddD of Id.t * Id.t
  | FSubD of Id.t * Id.t
  | FMulD of Id.t * Id.t
  | FDivD of Id.t * Id.t
  (* LdDF/StDF (x, V(y), d) : 浮動小数点をメモリのx+d*y番地{から読み込み / へ保存} *)
  (* LdDF/StDF (x, C(y), d) : 浮動小数点をメモリのx+y+d番地{から読み込み / へ保存} *)
  | LdDF of Id.t * id_or_imm * int
  | StDF of Id.t * Id.t * id_or_imm * int
  | Comment of string
  (* virtual instructions *)
  | IfEq of Id.t * id_or_imm * t * t
  | IfLE of Id.t * id_or_imm * t * t
  | IfGE of Id.t * id_or_imm * t * t (* 左右対称ではないので必要 *)
  | IfFEq of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 (caml2html: sparcasm_save) *)
  | Restore of Id.t (* スタック変数から値を復元 (caml2html: sparcasm_restore) *)
type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
(* プログラム全体 = 浮動小数点数テーブル + トップレベル関数 + メインの式 (caml2html: sparcasm_prog) *)
type prog = Prog of (Id.l * float) list * fundef list * t

(* floatに対するLet式を返す *)
(* floatを特別扱いする場合はここを編集する *)
let fletd(x, e1, e2) = Let((x, Type.Float), e1, e2)

(* "e1; e2"に対応する命令を返す *)
let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = (* Array.init 16 (fun i -> Printf.sprintf "%%r%d" i) *)
  [| "%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi" |]
let fregs = Array.init 8 (fun i -> Printf.sprintf "%%xmm%d" i)
let allregs = Array.to_list regs
let allfregs = Array.to_list fregs
let reg_cl = regs.(Array.length regs - 1) (* closure address (caml2html: sparcasm_regcl) *)
(*
let reg_sw = regs.(Array.length regs - 1) (* temporary for swap *)
let reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *)
*)
let reg_sp = "%ebp" (* stack pointer *)
let reg_hp = "min_caml_hp" (* heap pointer (caml2html: sparcasm_reghp) *)
(* let reg_ra = "%eax" (* return address *) *)
let is_reg x = (x.[0] = '%' || x = reg_hp)

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let fv_id_or_imm = function V(x) -> [x] | _ -> []
let rec fv_exp = function
  | Nop | Set(_) | SetL(_) | Comment(_) | Restore(_) -> []
  | Mov(x) | Neg(x) | FMovD(x) | FNegD(x) | Save(x, _) -> [x]
  | Add(x, y') | Sub(x, y') | Ld(x, y', _) | LdDF(x, y', _) -> x :: fv_id_or_imm y'
  | St(x, y, z', _) | StDF(x, y, z', _) -> x :: y :: fv_id_or_imm z'
  | FAddD(x, y) | FSubD(x, y) | FMulD(x, y) | FDivD(x, y) -> [x; y]
  | IfEq(x, y', e1, e2) | IfLE(x, y', e1, e2) | IfGE(x, y', e1, e2) -> x :: fv_id_or_imm y' @ remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | IfFEq(x, y, e1, e2) | IfFLE(x, y, e1, e2) -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | CallCls(x, ys, zs) -> x :: ys @ zs
  | CallDir(_, ys, zs) -> ys @ zs
and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) ->
    fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

(* "let x1 = e1 in e2" に相当するAsm.tの値を返す *)
let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp) -> Let(xt, exp, e2)
  | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)

let align i = (if i mod 8 = 0 then i else i + 4)

(* ioi = id_or_imm *)
let format_string_of_ioi = function
  | V id -> unary "Var"id
  | C i -> unary "Int" (string_of_int i)

let rec format_string_of_asm = function
  | Ans exp -> unary "Ans" (format_string_of_exp exp)
  | Let ((id, _), exp, asm) ->
    Printf.sprintf "@[<v 0>Let (@[<0>%s,@ %s,@]@ %s)@]"
      (unary "Var" id)
      (format_string_of_exp exp)
      (format_string_of_asm asm)

and format_string_of_exp  = function
  | Nop -> "@[<1>Nop@]"
  | Set i -> Printf.sprintf "@[<1>Int@ %d@]" i
  | SetL (Id.L id) -> unary "SetL" (unary "Var" id)
  | Mov id -> unary "Mov" (unary "Var" id)
  | Neg id -> unary "Neg" (unary "Var" id)
  | Add (id, ioi) -> binary "Add" (unary "Var" id) (format_string_of_ioi ioi)
  | Sub (id, ioi) -> binary "Sub" (unary "Var" id) (format_string_of_ioi ioi)
  | Ld (id, ioi, i) ->
    ternary "Ld"
      (unary "Var" id)
      (format_string_of_ioi ioi)
      (unary "Int" (string_of_int i))
  | St (id1, id2, ioi, i) ->
    quaternary "St"
      (unary "Var" id1)
      (unary "Var" id2)
      (format_string_of_ioi ioi)
      (unary "Int" (string_of_int i))
  | FMovD id -> unary "FMovD" (unary "Var" id)
  | FNegD id -> unary "FNegD" (unary "Var" id)
  | FAddD (id1, id2) -> binary "FAddD" (unary "Var" id1) (unary "Var" id2)
  | FSubD (id1, id2) -> binary "FSubD" (unary "Var" id1) (unary "Var" id2)
  | FMulD (id1, id2) -> binary "FMulD" (unary "Var" id1) (unary "Var" id2)
  | FDivD (id1, id2) -> binary "FDivD" (unary "Var" id1) (unary "Var" id2)
  | LdDF (id, ioi, i) ->
    ternary "LdDF" (unary "Var" id) (format_string_of_ioi ioi) (unary "Int" (string_of_int i))
  | StDF (id1, id2, ioi, i) ->
    quaternary "StDF"
      (unary "Var" id1)
      (unary "Var" id2)
      (format_string_of_ioi ioi)
      (unary "Int" (string_of_int i))
  | Comment str -> unary "Coment" str
  | IfEq (id, ioi, asm1, asm2) ->
    quaternary "IfEq"
      (unary "Var" id)
      (format_string_of_ioi ioi)
      (format_string_of_asm asm1)
      (format_string_of_asm asm2)
  | IfLE (id, ioi, asm1, asm2) ->
    quaternary "IfLE"
      (unary "Var" id)
      (format_string_of_ioi ioi)
      (format_string_of_asm asm1)
      (format_string_of_asm asm2)
  | IfGE (id, ioi, asm1, asm2) ->
    quaternary "IfGE"
      (unary "Var" id)
      (format_string_of_ioi ioi)
      (format_string_of_asm asm1)
      (format_string_of_asm asm2)
  | IfFEq (id1, id2, asm1, asm2) ->
    quaternary "IfFEq"
      (unary "Var" id1)
      (unary "Var" id2)
      (format_string_of_asm asm1)
      (format_string_of_asm asm2)
  | IfFLE (id1, id2, asm1, asm2) ->
    quaternary "IfFLE"
      (unary "Var" id1)
      (unary "Var" id2)
      (format_string_of_asm asm1)
      (format_string_of_asm asm2)
  | CallCls (id, idlist1, idlist2) ->
    ternary "CallCls"
      (unary "Var" id)
      (format_string_of_list idlist1 (unary "Var"))
      (format_string_of_list idlist2 (unary "Var"))
  | CallDir (Id.L id, idlist1, idlist2) ->
    ternary "CallCls"
      (unary "Var" id)
      (format_string_of_list idlist1 (unary "Var"))
      (format_string_of_list idlist2 (unary "Var"))
  | Save (id1, id2) -> binary "Save" (unary "Var" id1) (unary "Var" id2)
  | Restore id -> unary "Restore" (unary "Var" id)

and format_string_of_fundef
    {name=Id.L name;
     args=args;
     fargs=fargs;
     body=body;
     ret=_} =
  Printf.sprintf "@[<0>(@[<hv 0>%s,@ %s,@ %s,@ %s)@]@]"
    (quoted name)
    (format_string_of_list args quoted)
    (format_string_of_list fargs quoted)
    (format_string_of_asm body)

let format_string (Prog (ftable, fundefs, asm)) =
  ternary "Prog"
    (format_string_of_list ftable
       (fun (Id.L id, f) -> Printf.sprintf "(%s, %f)" id f))
    (format_string_of_list fundefs format_string_of_fundef)
    (format_string_of_asm asm)

let print fmt k =
  k
  |> format_string
  |> Js_string.replaceByRe [%re "/%/g"] "%%"
  |> (fun str -> Scanf.format_from_string str "")
  |> Format.fprintf fmt



let (||>) x f = f x; x

let string k =
  k
  |> format_string
  |> Js_string.replaceByRe [%re "/%/g"] "%%"
  |> (fun str -> Scanf.format_from_string str "")
  |> Format.sprintf
