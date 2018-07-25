(* 文字列を受け取り、コンパイルを各フェーズまで進める関数 *)

(* コンパイルだけ *)
let syntax str =
  (* エラーの解析のためにbufをとっておく必要がある *)
  let buf = Lexing.from_string str in
  try Parser.exp Lexer.token buf
  with Error.ParseError (start, end_) ->
    Error.explain_parse_error buf start end_
let type_ str    = syntax   str |> Typing.f
let knormal str  = type_    str |> KNormal.f
let alpha str    = knormal  str |> Alpha.f
let closure str  = alpha    str |> Closure.f
let virtual_ str = closure  str |> Virtual.f
let simm str     = virtual_ str |> Simm.f
let regalloc str = simm     str |> RegAlloc.f
let emit str     = regalloc str |> Emit.f stdout
