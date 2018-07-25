open FormatUtil

type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

let rec format_string = function
  | Unit -> "@[unit@]"
  | Bool -> "@[bool@]"
  | Int -> "@[int@]"
  | Float -> "@[float@]"
  | Fun (tlist, t) -> binary "Fun" (format_string_of_list tlist format_string) (format_string t)
  | Tuple tlist -> unary "Tuple" (format_string_of_list tlist format_string)
  | Array t -> unary "Array" (format_string t)
  | Var t_op -> unary "Var" (match !t_op with  None -> "None" | Some t -> format_string t)

let print t =
  t
  |> format_string
  |> (fun str -> Scanf.format_from_string str "")
  |> Format.printf

let string t =
  t
  |> format_string
  |> (fun str -> Scanf.format_from_string str "")
  |> Format.sprintf
