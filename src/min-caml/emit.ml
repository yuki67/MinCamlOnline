open Asm

(* external gethi : float -> int32 = "gethi"
 * external getlo : float -> int32 = "getlo" *)

let output_string = ref ""

let add_output str =
  output_string := !output_string ^ str

let gethi f =
  let i = Int64.bits_of_float f in
  Int64.to_int32 (Int64.shift_right_logical i 32)

let getlo f =
  let i = Int64.bits_of_float f in
  Int64.to_int32 (Int64.shift_right_logical (Int64.shift_left i 32) 32)

let stackset = ref S.empty (* すでにSaveされた変数の集合 (caml2html: emit_stackset) *)
let stackmap = ref [] (* Saveされた変数の、スタックにおける位置 (caml2html: emit_stackmap) *)
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad =
       if List.length !stackmap mod 2 = 0 then [] else [Id.gentmp Type.Int] in
     stackmap := !stackmap @ pad @ [x; x])
let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
  loc !stackmap
let offset x = 4 * List.hd (locate x)
let stacksize () = align (List.length !stackmap * 4)

let pp_id_or_imm = function
  | V(x) -> x
  | C(i) -> "$" ^ string_of_int i

(* 関数呼び出しのために引数を並べ替える(register shuffling) (caml2html: emit_shuffle) *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] -> (* no acyclic moves; resolve a cyclic move *)
    (y, sw) :: (x, y) :: shuffle sw (List.map
                                       (function
                                         | (y', z) when y = y' -> (sw, z)
                                         | yz -> yz)
                                       xys)
  | xys, acyc -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t (* 末尾かどうかを表すデータ型 (caml2html: emit_dest) *)
let rec g = function (* 命令列のアセンブリ生成 (caml2html: emit_g) *)
  | dest, Ans(exp) -> g' (dest, exp)
  | dest, Let((x, t), exp, e) ->
    g' (NonTail(x), exp);
    g (dest, e)
and g' = function (* 各命令のアセンブリ生成 (caml2html: emit_gprime) *)
  (* 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail) *)
  | NonTail(_), Nop -> ()
  | NonTail(x), Set(i) -> Printf.sprintf "\tmovl\t$%d, %s\n" i x |> add_output
  | NonTail(x), SetL(Id.L(y)) -> Printf.sprintf "\tmovl\t$%s, %s\n" y x |> add_output
  | NonTail(x), Mov(y) ->
    if x <> y then Printf.sprintf "\tmovl\t%s, %s\n" y x |> add_output
  | NonTail(x), Neg(y) ->
    if x <> y then Printf.sprintf "\tmovl\t%s, %s\n" y x |> add_output;
    Printf.sprintf "\tnegl\t%s\n" x |> add_output
  | NonTail(x), Add(y, z') ->
    if V(x) = z' then
      Printf.sprintf "\taddl\t%s, %s\n" y x |> add_output
    else
      (if x <> y then Printf.sprintf "\tmovl\t%s, %s\n" y x |> add_output;
       Printf.sprintf "\taddl\t%s, %s\n" (pp_id_or_imm z') x) |> add_output
  | NonTail(x), Sub(y, z') ->
    if V(x) = z' then
      (Printf.sprintf "\tsubl\t%s, %s\n" y x |> add_output;
       Printf.sprintf "\tnegl\t%s\n" x) |> add_output
    else
      (if x <> y then Printf.sprintf "\tmovl\t%s, %s\n" y x |> add_output;
       Printf.sprintf "\tsubl\t%s, %s\n" (pp_id_or_imm z') x |> add_output)
  | NonTail(x), Ld(y, V(z), i) -> Printf.sprintf "\tmovl\t(%s,%s,%d), %s\n" y z i x |> add_output
  | NonTail(x), Ld(y, C(j), i) -> Printf.sprintf "\tmovl\t%d(%s), %s\n" (j * i) y x |> add_output
  | NonTail(_), St(x, y, V(z), i) -> Printf.sprintf "\tmovl\t%s, (%s,%s,%d)\n" x y z i |> add_output
  | NonTail(_), St(x, y, C(j), i) -> Printf.sprintf "\tmovl\t%s, %d(%s)\n" x (j * i) y |> add_output
  | NonTail(x), FMovD(y) ->
    if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> add_output
  | NonTail(x), FNegD(y) ->
    if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> add_output;
    Printf.sprintf "\txorpd\tmin_caml_fnegd, %s\n" x |> add_output
  | NonTail(x), FAddD(y, z) ->
    if x = z then
      Printf.sprintf "\taddsd\t%s, %s\n" y x |> add_output
    else
      (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> add_output;
       Printf.sprintf "\taddsd\t%s, %s\n" z x |> add_output)
  | NonTail(x), FSubD(y, z) ->
    if x = z then (* [XXX] ugly *)
      let ss = stacksize () in
      Printf.sprintf "\tmovsd\t%s, %d(%s)\n" z ss reg_sp |> add_output;
      if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> add_output;
      Printf.sprintf "\tsubsd\t%d(%s), %s\n" ss reg_sp x |> add_output
    else
      (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> add_output;
       Printf.sprintf "\tsubsd\t%s, %s\n" z x |> add_output)
  | NonTail(x), FMulD(y, z) ->
    if x = z then
      Printf.sprintf "\tmulsd\t%s, %s\n" y x |> add_output
    else
      (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> add_output;
       Printf.sprintf "\tmulsd\t%s, %s\n" z x |> add_output)
  | NonTail(x), FDivD(y, z) ->
    if x = z then (* [XXX] ugly *)
      let ss = stacksize () in
      Printf.sprintf "\tmovsd\t%s, %d(%s)\n" z ss reg_sp |> add_output;
      if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> add_output;
      Printf.sprintf "\tdivsd\t%d(%s), %s\n" ss reg_sp x |> add_output
    else
      (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x |> add_output;
       Printf.sprintf "\tdivsd\t%s, %s\n" z x |> add_output)
  | NonTail(x), LdDF(y, V(z), i) -> Printf.sprintf "\tmovsd\t(%s,%s,%d), %s\n" y z i x |> add_output
  | NonTail(x), LdDF(y, C(j), i) -> Printf.sprintf "\tmovsd\t%d(%s), %s\n" (j * i) y x |> add_output
  | NonTail(_), StDF(x, y, V(z), i) -> Printf.sprintf "\tmovsd\t%s, (%s,%s,%d)\n" x y z i |> add_output
  | NonTail(_), StDF(x, y, C(j), i) -> Printf.sprintf "\tmovsd\t%s, %d(%s)\n" x (j * i) y |> add_output
  | NonTail(_), Comment(s) -> Printf.sprintf "\t# %s\n" s |> add_output
  (* 退避の仮想命令の実装 (caml2html: emit_save) *)
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
    save y;
    Printf.sprintf "\tmovl\t%s, %d(%s)\n" x (offset y) reg_sp |> add_output
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
    savef y;
    Printf.sprintf "\tmovsd\t%s, %d(%s)\n" x (offset y) reg_sp |> add_output
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  (* 復帰の仮想命令の実装 (caml2html: emit_restore) *)
  | NonTail(x), Restore(y) when List.mem x allregs ->
    Printf.sprintf "\tmovl\t%d(%s), %s\n" (offset y) reg_sp x |> add_output
  | NonTail(x), Restore(y) ->
    assert (List.mem x allfregs);
    Printf.sprintf "\tmovsd\t%d(%s), %s\n" (offset y) reg_sp x |> add_output
  (* 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret) *)
  | Tail, (Nop | St _ | StDF _ | Comment _ | Save _ as exp) ->
    g' (NonTail(Id.gentmp Type.Unit), exp);
    Printf.sprintf "\tret\n" |> add_output;
  | Tail, (Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
    g' (NonTail(regs.(0)), exp);
    Printf.sprintf "\tret\n" |> add_output;
  | Tail, (FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _ | LdDF _  as exp) ->
    g' (NonTail(fregs.(0)), exp);
    Printf.sprintf "\tret\n" |> add_output;
  | Tail, (Restore(x) as exp) ->
    (match locate x with
     | [i] -> g' (NonTail(regs.(0)), exp)
     | [i; j] when i + 1 = j -> g' (NonTail(fregs.(0)), exp)
     | _ -> assert false);
    Printf.sprintf "\tret\n" |> add_output;
  | Tail, IfEq(x, y', e1, e2) ->
    Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x |> add_output;
    g'_tail_if e1 e2 "je" "jne"
  | Tail, IfLE(x, y', e1, e2) ->
    Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x |> add_output;
    g'_tail_if e1 e2 "jle" "jg"
  | Tail, IfGE(x, y', e1, e2) ->
    Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x |> add_output;
    g'_tail_if e1 e2 "jge" "jl"
  | Tail, IfFEq(x, y, e1, e2) ->
    Printf.sprintf "\tcomisd\t%s, %s\n" y x |> add_output;
    g'_tail_if e1 e2 "je" "jne"
  | Tail, IfFLE(x, y, e1, e2) ->
    Printf.sprintf "\tcomisd\t%s, %s\n" y x |> add_output;
    g'_tail_if e1 e2 "jbe" "ja"
  | NonTail(z), IfEq(x, y', e1, e2) ->
    Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x |> add_output;
    g'_non_tail_if (NonTail(z)) e1 e2 "je" "jne"
  | NonTail(z), IfLE(x, y', e1, e2) ->
    Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x |> add_output;
    g'_non_tail_if (NonTail(z)) e1 e2 "jle" "jg"
  | NonTail(z), IfGE(x, y', e1, e2) ->
    Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x |> add_output;
    g'_non_tail_if (NonTail(z)) e1 e2 "jge" "jl"
  | NonTail(z), IfFEq(x, y, e1, e2) ->
    Printf.sprintf "\tcomisd\t%s, %s\n" y x |> add_output;
    g'_non_tail_if (NonTail(z)) e1 e2 "je" "jne"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
    Printf.sprintf "\tcomisd\t%s, %s\n" y x |> add_output;
    g'_non_tail_if (NonTail(z)) e1 e2 "jbe" "ja"
  (* 関数呼び出しの仮想命令の実装 (caml2html: emit_call) *)
  | Tail, CallCls(x, ys, zs) -> (* 末尾呼び出し (caml2html: emit_tailcall) *)
    g'_args [(x, reg_cl)] ys zs;
    Printf.sprintf "\tjmp\t*(%s)\n" reg_cl |> add_output;
  | Tail, CallDir(Id.L(x), ys, zs) -> (* 末尾呼び出し *)
    g'_args [] ys zs;
    Printf.sprintf "\tjmp\t%s\n" x |> add_output;
  | NonTail(a), CallCls(x, ys, zs) ->
    g'_args [(x, reg_cl)] ys zs;
    let ss = stacksize () in
    if ss > 0 then Printf.sprintf "\taddl\t$%d, %s\n" ss reg_sp |> add_output;
    Printf.sprintf "\tcall\t*(%s)\n" reg_cl |> add_output;
    if ss > 0 then Printf.sprintf "\tsubl\t$%d, %s\n" ss reg_sp |> add_output;
    if List.mem a allregs && a <> regs.(0) then
      Printf.sprintf "\tmovl\t%s, %s\n" regs.(0) a |> add_output
    else if List.mem a allfregs && a <> fregs.(0) then
      Printf.sprintf "\tmovsd\t%s, %s\n" fregs.(0) a |> add_output
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
    g'_args [] ys zs;
    let ss = stacksize () in
    if ss > 0 then Printf.sprintf "\taddl\t$%d, %s\n" ss reg_sp |> add_output;
    Printf.sprintf "\tcall\t%s\n" x |> add_output;
    if ss > 0 then Printf.sprintf "\tsubl\t$%d, %s\n" ss reg_sp |> add_output;
    if List.mem a allregs && a <> regs.(0) then
      Printf.sprintf "\tmovl\t%s, %s\n" regs.(0) a |> add_output
    else if List.mem a allfregs && a <> fregs.(0) then
      Printf.sprintf "\tmovsd\t%s, %s\n" fregs.(0) a |> add_output
and g'_tail_if e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  Printf.sprintf "\t%s\t%s\n" bn b_else |> add_output;
  let stackset_back = !stackset in
  g (Tail, e1);
  Printf.sprintf "%s:\n" b_else |> add_output;
  stackset := stackset_back;
  g (Tail, e2)
and g'_non_tail_if dest e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.sprintf "\t%s\t%s\n" bn b_else |> add_output;
  let stackset_back = !stackset in
  g (dest, e1);
  let stackset1 = !stackset in
  Printf.sprintf "\tjmp\t%s\n" b_cont |> add_output;
  Printf.sprintf "%s:\n" b_else |> add_output;
  stackset := stackset_back;
  g (dest, e2);
  Printf.sprintf "%s:\n" b_cont |> add_output;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_args x_reg_cl ys zs =
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let sw = Printf.sprintf "%d(%s)" (stacksize ()) reg_sp in
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> Printf.sprintf "\tmovl\t%s, %s\n" y r |> add_output)
    (shuffle sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> Printf.sprintf "\tmovsd\t%s, %s\n" z fr |> add_output)
    (shuffle sw zfrs)

let h { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.sprintf "%s:\n" x |> add_output;
  stackset := S.empty;
  stackmap := [];
  g (Tail, e)

let f (Prog(data, fundefs, e)) =
  output_string := "";

  Printf.sprintf ".data\n" |> add_output;
  Printf.sprintf ".balign\t8\n" |> add_output;
  List.iter
    (fun (Id.L(x), d) ->
       Printf.sprintf "%s:\t# %f\n" x d |> add_output;
       Printf.sprintf "\t.long\t0x%lx\n" (gethi d) |> add_output;
       Printf.sprintf "\t.long\t0x%lx\n" (getlo d) |> add_output)
    data;
  Printf.sprintf ".text\n" |> add_output;
  List.iter (fun fundef -> h fundef) fundefs;
  Printf.sprintf ".globl\tmin_caml_start\n" |> add_output;
  Printf.sprintf "min_caml_start:\n" |> add_output;
  Printf.sprintf ".globl\t_min_caml_start\n" |> add_output;
  Printf.sprintf "_min_caml_start: # for cygwin\n" |> add_output;
  Printf.sprintf "\tpushl\t%%eax\n" |> add_output;
  Printf.sprintf "\tpushl\t%%ebx\n" |> add_output;
  Printf.sprintf "\tpushl\t%%ecx\n" |> add_output;
  Printf.sprintf "\tpushl\t%%edx\n" |> add_output;
  Printf.sprintf "\tpushl\t%%esi\n" |> add_output;
  Printf.sprintf "\tpushl\t%%edi\n" |> add_output;
  Printf.sprintf "\tpushl\t%%ebp\n" |> add_output;
  Printf.sprintf "\tmovl\t32(%%esp),%s\n" reg_sp |> add_output;
  Printf.sprintf "\tmovl\t36(%%esp),%s\n" regs.(0) |> add_output;
  Printf.sprintf "\tmovl\t%s,%s\n" regs.(0) reg_hp |> add_output;
  stackset := S.empty;
  stackmap := [];
  g (NonTail(regs.(0)), e);
  Printf.sprintf "\tpopl\t%%ebp\n" |> add_output;
  Printf.sprintf "\tpopl\t%%edi\n" |> add_output;
  Printf.sprintf "\tpopl\t%%esi\n" |> add_output;
  Printf.sprintf "\tpopl\t%%edx\n" |> add_output;
  Printf.sprintf "\tpopl\t%%ecx\n" |> add_output;
  Printf.sprintf "\tpopl\t%%ebx\n" |> add_output;
  Printf.sprintf "\tpopl\t%%eax\n" |> add_output;
  Printf.sprintf "\tret\n" |> add_output;

  !output_string
