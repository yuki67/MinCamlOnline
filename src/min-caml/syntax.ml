open FormatUtil

type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec format_string = function
  | Unit -> "@[<1>()@]"
  | Bool b -> Printf.sprintf "@[<1>Bool %s@]" (string_of_bool b)
  | Int i -> Printf.sprintf "@[<1>Int %d@]" i
  | Float f -> Printf.sprintf "@[<1>Float %f@]" f
  | Not s -> unary "Not" (format_string s)
  | Neg s -> unary "Neg" (format_string s)
  | Add (s1, s2) -> binary "Add" (format_string s1) (format_string s2)
  | Sub (s1, s2) -> binary "Sub" (format_string s1) (format_string s2)
  | FNeg s -> unary "FNeg" (format_string s)
  | FAdd (s1, s2) -> binary "FAdd" (format_string s1) (format_string s2)
  | FSub (s1, s2) -> binary "FSub" (format_string s1) (format_string s2)
  | FMul (s1, s2) -> binary "FMul" (format_string s1) (format_string s2)
  | FDiv (s1, s2) -> binary "FDiv" (format_string s1) (format_string s2)
  | Eq (s1, s2) -> binary "Eq" (format_string s1) (format_string s2)
  | LE (s1, s2) -> binary "LE" (format_string s1) (format_string s2)
  | If (s1, s2, s3) ->
    ternary "If" (format_string s1) (format_string s2) (format_string s3)
  | Let ((id, _), s1, s2) ->
    Printf.sprintf "@[<v 0>Let (@[<0>%s,@ %s,@]@ %s)@]"
      (quoted id)
      (format_string s1)
      (format_string s2)
  | Var id -> unary "Var" id;
  | LetRec (def, s) ->
    Printf.sprintf "@[<v 0>Let (@[<0>%s,@ %s,@ %s,@]@ %s)@]"
      (quoted (fst def.name))
      (format_string_of_list (List.map fst def.args) quoted)
      (format_string def.body)
      (format_string s)
  | App (s, slist) ->
    binary "App"
      (format_string s)
      (format_string_of_list slist format_string)
  | Tuple slist ->
    unary "Tuple" (format_string_of_list slist format_string)
  | LetTuple (alist, s1, s2) ->
    Printf.sprintf "@[<v 0>Let (@[<0>%s,@ %s,@]@ %s)@]"
      (format_string_of_list (List.map fst alist) quoted)
      (format_string s1)
      (format_string s2)
  | Array (s1, s2) -> binary "Array" (format_string s1) (format_string s2)
  | Get (s1, s2) -> binary "Get" (format_string s1) (format_string s2)
  | Put (s1, s2, s3) ->
    ternary "Put" (format_string s1) (format_string s2) (format_string s3)

let string s =
  s
  |> format_string
  |> (fun str -> Scanf.format_from_string str "")
  |> Format.sprintf

let print s =
  s
  |> format_string
  |> (fun str -> Scanf.format_from_string str "")
  |> Format.printf
