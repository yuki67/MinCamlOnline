let limit = ref 1000

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
    let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
    if e = e' then e else
      iter (n - 1) e'

let lexbuf _ l = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  print_string
    (Emit.f
       (RegAlloc.f
          (Simm.f
             (Virtual.f
                (Closure.f
                   (iter !limit
                      (Alpha.f
                         (KNormal.f
                            (Typing.f
                               (Parser.exp Lexer.token l))))))))))

let string s = lexbuf stdout (Lexing.from_string s) (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  let buf = Lexing.from_channel inchan in
  try
    lexbuf outchan buf;
    close_in inchan;
    close_out outchan;
  with
    Error.ParseError (start, end_) ->
    (close_in inchan;
     close_out outchan;
     Error.explain_parse_error buf start end_)

(* let () = (\* ここからコンパイラの実行が開始される (caml2html: main_entry) *\)
 *   let files = ref [] in
 *   Arg.parse
 *     [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
 *      ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated")]
 *     (fun s -> files := !files @ [s])
 *     ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
 *      Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
 *   List.iter
 *     (fun f -> ignore (file f))
 *     !files *)
