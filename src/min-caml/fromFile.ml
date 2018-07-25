(* ファイル名を受け取り、コンパイルを各フェーズまで進める関数 *)

(* コンパイルだけ *)
let syntax filename =
  let inchan = open_in filename in
  (* エラーの解析のためにbufをとっておく必要がある *)
  let buf = Lexing.from_channel inchan in
  let syntax = try Parser.exp Lexer.token buf
    with Error.ParseError (start, end_) ->
      Error.explain_parse_error buf start end_ in
  close_in inchan;
  syntax
let type_    str = syntax   str |> Typing.f
let knormal  str = type_    str |> KNormal.f
let alpha    str = knormal  str |> Alpha.f
let closure  str = alpha    str |> Closure.f
let virtual_ str = closure  str |> Virtual.f
let simm     str = virtual_ str |> Simm.f
let regalloc str = simm     str |> RegAlloc.f
let emit     str = regalloc str |> Emit.f stdout
