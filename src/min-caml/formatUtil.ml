(* 各モジュールのprint関数で利用する補助関数 *)

let format_string_of_list lst printer =
  let rec loop acc = function
    | [] -> acc
    | [x] -> acc ^ printer x
    | x::xs -> loop (printer x ^ ";@ " ^ acc) xs in
  "[@[<0>" ^ loop "" lst ^ "@]]"

(* ""でおおう: hoge -> "hoge" *)
let quoted str =
  Printf.sprintf "@[\"%s\"@]" str

(* n項演算子: 適切にBoxを配置する *)
let unary name str =
  Printf.sprintf "@[<1>%s@ %s@]"
    name str
let binary name str1 str2 =
  Printf.sprintf "@[<1>%s@ (@[<hv 0>%s,@ %s)@]@]"
    name str1 str2
let ternary name str1 str2 str3 =
  Printf.sprintf "@[<1>%s@ (@[<hv 0>%s,@ %s,@ %s)@]@]"
    name str1 str2 str3
let quaternary name str1 str2 str3 str4 =
  Printf.sprintf "@[<1>%s@ (@[<hv 0>%s,@ %s,@ %s,@ %s)@]@]"
    name str1 str2 str3 str4
