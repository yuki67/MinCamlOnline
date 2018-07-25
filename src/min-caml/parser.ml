type token =
  | BOOL of (bool)
  | INT of (int)
  | FLOAT of (float)
  | NOT
  | MINUS
  | PLUS
  | MINUS_DOT
  | PLUS_DOT
  | AST_DOT
  | SLASH_DOT
  | EQUAL
  | LESS_GREATER
  | LESS_EQUAL
  | GREATER_EQUAL
  | LESS
  | GREATER
  | IF
  | THEN
  | ELSE
  | IDENT of (Id.t)
  | LET
  | IN
  | REC
  | COMMA
  | ARRAY_CREATE
  | DOT
  | LESS_MINUS
  | SEMICOLON
  | LPAREN
  | RPAREN
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
(* parserが利用する変数、関数、型などの定義 *)
open Syntax
(* 「式xの型は未確定である」を表す組を返す*)
let addtyp x = (x, Type.gentyp ())
# 42 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
let yytransl_const = [|
  260 (* NOT *);
  261 (* MINUS *);
  262 (* PLUS *);
  263 (* MINUS_DOT *);
  264 (* PLUS_DOT *);
  265 (* AST_DOT *);
  266 (* SLASH_DOT *);
  267 (* EQUAL *);
  268 (* LESS_GREATER *);
  269 (* LESS_EQUAL *);
  270 (* GREATER_EQUAL *);
  271 (* LESS *);
  272 (* GREATER *);
  273 (* IF *);
  274 (* THEN *);
  275 (* ELSE *);
  277 (* LET *);
  278 (* IN *);
  279 (* REC *);
  280 (* COMMA *);
  281 (* ARRAY_CREATE *);
  282 (* DOT *);
  283 (* LESS_MINUS *);
  284 (* SEMICOLON *);
  285 (* LPAREN *);
  286 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* BOOL *);
  258 (* INT *);
  259 (* FLOAT *);
  276 (* IDENT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\007\000\007\000\006\000\006\000\005\000\005\000\
\004\000\004\000\000\000"

let yylen = "\002\000\
\003\000\002\000\001\000\001\000\001\000\001\000\005\000\001\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\005\000\
\006\000\008\000\003\000\001\000\006\000\002\000\003\000\007\000\
\001\000\004\000\002\000\001\000\002\000\001\000\003\000\003\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\033\000\003\000\004\000\005\000\000\000\000\000\
\000\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\011\000\010\000\000\000\000\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\016\000\017\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\042\000\041\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\015\000\016\000\049\000\051\000\017\000\044\000\075\000"

let yysindex = "\011\000\
\107\255\000\000\000\000\000\000\000\000\000\000\107\255\107\255\
\107\255\107\255\000\000\236\254\065\255\076\255\102\002\003\255\
\239\254\000\000\000\000\000\000\214\255\006\255\255\254\002\255\
\097\255\000\000\136\255\107\255\107\255\107\255\107\255\107\255\
\107\255\107\255\107\255\107\255\107\255\107\255\107\255\107\255\
\107\255\251\254\000\255\065\255\107\255\107\255\107\255\005\255\
\011\255\004\255\234\254\254\254\000\255\000\000\001\255\001\255\
\001\255\001\255\000\000\000\000\049\255\049\255\049\255\049\255\
\049\255\049\255\239\255\102\002\107\255\000\255\239\255\030\002\
\054\002\005\255\037\255\107\255\030\255\040\255\051\255\107\255\
\162\255\107\255\107\255\000\000\107\255\102\002\000\000\000\000\
\107\255\188\255\036\255\126\002\102\002\102\002\078\002\000\000\
\107\255\107\255\126\002\102\002"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\064\000\087\000\
\165\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\031\000\113\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\139\000\000\000\191\000\217\000\
\243\000\018\001\000\000\000\000\048\001\078\001\104\001\130\001\
\156\001\182\001\189\001\035\001\000\000\061\000\208\001\000\000\
\000\000\054\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\065\001\000\000\000\000\
\000\000\000\000\001\000\226\001\242\001\047\255\000\000\000\000\
\000\000\000\000\240\001\004\002"

let yygindex = "\000\000\
\006\000\005\000\000\000\000\000\000\000\000\000\252\255"

let yytablesize = 918
let yytable = "\022\000\
\007\000\078\000\023\000\004\000\005\000\006\000\045\000\079\000\
\024\000\032\000\033\000\001\000\018\000\019\000\020\000\021\000\
\047\000\025\000\048\000\027\000\043\000\050\000\011\000\069\000\
\074\000\052\000\080\000\077\000\042\000\053\000\038\000\014\000\
\076\000\055\000\056\000\057\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\085\000\
\070\000\087\000\071\000\072\000\073\000\028\000\029\000\030\000\
\031\000\032\000\033\000\088\000\037\000\089\000\097\000\043\000\
\036\000\004\000\005\000\006\000\034\000\084\000\000\000\000\000\
\000\000\000\000\081\000\003\000\004\000\005\000\006\000\007\000\
\008\000\086\000\009\000\000\000\011\000\090\000\008\000\092\000\
\093\000\000\000\094\000\000\000\010\000\014\000\095\000\011\000\
\012\000\004\000\005\000\006\000\013\000\000\000\099\000\100\000\
\014\000\026\000\003\000\004\000\005\000\006\000\007\000\008\000\
\030\000\009\000\000\000\000\000\011\000\000\000\000\000\000\000\
\000\000\000\000\052\000\010\000\000\000\014\000\011\000\012\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\014\000\
\000\000\000\000\031\000\000\000\028\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\040\000\
\000\000\000\000\000\000\041\000\028\000\054\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\040\000\000\000\000\000\000\000\041\000\013\000\091\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\000\000\041\000\
\012\000\096\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\000\000\046\000\
\000\000\000\000\000\000\000\000\000\000\040\000\000\000\000\000\
\000\000\041\000\015\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\000\000\
\000\000\007\000\007\000\007\000\000\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\014\000\007\000\007\000\007\000\000\000\007\000\000\000\
\007\000\000\000\007\000\000\000\007\000\007\000\007\000\038\000\
\038\000\038\000\027\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\018\000\
\038\000\038\000\038\000\000\000\038\000\000\000\038\000\000\000\
\000\000\000\000\038\000\038\000\038\000\037\000\037\000\037\000\
\024\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\037\000\021\000\037\000\037\000\
\037\000\000\000\037\000\000\000\037\000\000\000\000\000\000\000\
\037\000\037\000\037\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\019\000\
\008\000\008\000\000\000\000\000\008\000\000\000\008\000\000\000\
\000\000\000\000\008\000\000\000\008\000\030\000\030\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\020\000\030\000\030\000\000\000\000\000\030\000\000\000\
\030\000\000\000\000\000\000\000\030\000\000\000\030\000\031\000\
\031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
\031\000\031\000\031\000\022\000\031\000\031\000\000\000\000\000\
\031\000\000\000\031\000\000\000\000\000\000\000\031\000\000\000\
\031\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\028\000\028\000\028\000\023\000\028\000\028\000\
\000\000\000\000\028\000\000\000\040\000\000\000\000\000\000\000\
\028\000\000\000\028\000\013\000\013\000\013\000\013\000\000\000\
\000\000\013\000\013\000\013\000\013\000\013\000\013\000\039\000\
\013\000\013\000\000\000\000\000\013\000\000\000\013\000\000\000\
\000\000\000\000\013\000\000\000\013\000\012\000\012\000\012\000\
\012\000\029\000\000\000\012\000\012\000\012\000\012\000\012\000\
\012\000\000\000\012\000\012\000\000\000\000\000\012\000\032\000\
\012\000\025\000\000\000\000\000\012\000\000\000\012\000\015\000\
\015\000\015\000\015\000\000\000\000\000\015\000\015\000\015\000\
\015\000\015\000\015\000\026\000\015\000\015\000\000\000\000\000\
\015\000\000\000\015\000\000\000\000\000\000\000\015\000\000\000\
\015\000\000\000\000\000\000\000\000\000\000\000\014\000\014\000\
\014\000\014\000\000\000\000\000\014\000\014\000\014\000\014\000\
\014\000\014\000\000\000\014\000\014\000\000\000\000\000\014\000\
\000\000\014\000\000\000\000\000\000\000\014\000\000\000\014\000\
\000\000\000\000\000\000\000\000\027\000\027\000\000\000\000\000\
\027\000\000\000\018\000\018\000\018\000\018\000\018\000\018\000\
\027\000\018\000\018\000\000\000\000\000\018\000\000\000\018\000\
\000\000\000\000\000\000\018\000\000\000\018\000\000\000\000\000\
\000\000\000\000\024\000\024\000\000\000\000\000\024\000\000\000\
\021\000\021\000\021\000\021\000\021\000\021\000\024\000\021\000\
\021\000\000\000\000\000\021\000\000\000\021\000\000\000\000\000\
\000\000\021\000\000\000\021\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\019\000\019\000\019\000\019\000\019\000\
\000\000\019\000\019\000\000\000\000\000\019\000\000\000\019\000\
\000\000\000\000\000\000\019\000\000\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\020\000\020\000\020\000\
\020\000\020\000\000\000\020\000\020\000\000\000\000\000\020\000\
\000\000\020\000\000\000\000\000\000\000\020\000\000\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\022\000\
\022\000\022\000\022\000\022\000\000\000\022\000\022\000\000\000\
\000\000\022\000\000\000\022\000\000\000\000\000\000\000\022\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\023\000\023\000\023\000\023\000\023\000\000\000\023\000\
\023\000\000\000\000\000\023\000\000\000\023\000\040\000\040\000\
\000\000\023\000\040\000\023\000\040\000\000\000\000\000\000\000\
\040\000\000\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\039\000\039\000\000\000\000\000\039\000\000\000\039\000\
\000\000\000\000\000\000\039\000\000\000\039\000\000\000\000\000\
\000\000\000\000\000\000\029\000\029\000\000\000\000\000\029\000\
\000\000\000\000\000\000\000\000\000\000\029\000\000\000\029\000\
\000\000\032\000\032\000\025\000\025\000\032\000\000\000\025\000\
\000\000\000\000\000\000\032\000\000\000\032\000\000\000\025\000\
\000\000\000\000\000\000\000\000\000\000\026\000\026\000\000\000\
\000\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\000\000\000\000\
\082\000\000\000\000\000\000\000\000\000\040\000\000\000\000\000\
\000\000\041\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\000\000\000\000\
\000\000\000\000\000\000\083\000\000\000\040\000\000\000\000\000\
\000\000\041\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\000\000\000\000\
\000\000\000\000\000\000\098\000\000\000\040\000\000\000\000\000\
\000\000\041\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\000\000\000\000\
\000\000\041\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000"

let yycheck = "\020\001\
\000\000\024\001\023\001\001\001\002\001\003\001\024\001\030\001\
\029\001\009\001\010\001\001\000\007\000\008\000\009\000\010\000\
\011\001\013\000\020\001\014\000\016\000\020\001\020\001\029\001\
\020\001\026\001\029\001\024\001\026\001\025\000\000\000\029\001\
\022\001\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\011\001\
\044\000\020\001\045\000\046\000\047\000\005\001\006\001\007\001\
\008\001\009\001\010\001\020\001\000\000\011\001\027\001\000\000\
\011\001\001\001\002\001\003\001\022\001\074\000\255\255\255\255\
\255\255\255\255\069\000\000\001\001\001\002\001\003\001\004\001\
\005\001\076\000\007\001\255\255\020\001\080\000\000\000\082\000\
\083\000\255\255\085\000\255\255\017\001\029\001\089\000\020\001\
\021\001\001\001\002\001\003\001\025\001\255\255\097\000\098\000\
\029\001\030\001\000\001\001\001\002\001\003\001\004\001\005\001\
\000\000\007\001\255\255\255\255\020\001\255\255\255\255\255\255\
\255\255\255\255\026\001\017\001\255\255\029\001\020\001\021\001\
\255\255\255\255\255\255\025\001\255\255\255\255\255\255\029\001\
\255\255\255\255\000\000\255\255\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\024\001\
\255\255\255\255\255\255\028\001\000\000\030\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\024\001\255\255\255\255\255\255\028\001\000\000\030\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\024\001\255\255\255\255\255\255\028\001\
\000\000\030\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\255\255\255\255\255\255\255\255\255\255\024\001\255\255\255\255\
\255\255\028\001\000\000\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\255\255\001\001\002\001\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\000\000\018\001\019\001\020\001\255\255\022\001\255\255\
\024\001\255\255\026\001\255\255\028\001\029\001\030\001\001\001\
\002\001\003\001\000\000\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\000\000\
\018\001\019\001\020\001\255\255\022\001\255\255\024\001\255\255\
\255\255\255\255\028\001\029\001\030\001\001\001\002\001\003\001\
\000\000\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\000\000\018\001\019\001\
\020\001\255\255\022\001\255\255\024\001\255\255\255\255\255\255\
\028\001\029\001\030\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\000\000\
\018\001\019\001\255\255\255\255\022\001\255\255\024\001\255\255\
\255\255\255\255\028\001\255\255\030\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\000\000\018\001\019\001\255\255\255\255\022\001\255\255\
\024\001\255\255\255\255\255\255\028\001\255\255\030\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\000\000\018\001\019\001\255\255\255\255\
\022\001\255\255\024\001\255\255\255\255\255\255\028\001\255\255\
\030\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\000\000\018\001\019\001\
\255\255\255\255\022\001\255\255\000\000\255\255\255\255\255\255\
\028\001\255\255\030\001\005\001\006\001\007\001\008\001\255\255\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\000\000\
\018\001\019\001\255\255\255\255\022\001\255\255\024\001\255\255\
\255\255\255\255\028\001\255\255\030\001\005\001\006\001\007\001\
\008\001\000\000\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\019\001\255\255\255\255\022\001\000\000\
\024\001\000\000\255\255\255\255\028\001\255\255\030\001\005\001\
\006\001\007\001\008\001\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\000\000\018\001\019\001\255\255\255\255\
\022\001\255\255\024\001\255\255\255\255\255\255\028\001\255\255\
\030\001\255\255\255\255\255\255\255\255\255\255\005\001\006\001\
\007\001\008\001\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\018\001\019\001\255\255\255\255\022\001\
\255\255\024\001\255\255\255\255\255\255\028\001\255\255\030\001\
\255\255\255\255\255\255\255\255\018\001\019\001\255\255\255\255\
\022\001\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\030\001\018\001\019\001\255\255\255\255\022\001\255\255\024\001\
\255\255\255\255\255\255\028\001\255\255\030\001\255\255\255\255\
\255\255\255\255\018\001\019\001\255\255\255\255\022\001\255\255\
\011\001\012\001\013\001\014\001\015\001\016\001\030\001\018\001\
\019\001\255\255\255\255\022\001\255\255\024\001\255\255\255\255\
\255\255\028\001\255\255\030\001\255\255\255\255\255\255\255\255\
\255\255\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\019\001\255\255\255\255\022\001\255\255\024\001\
\255\255\255\255\255\255\028\001\255\255\030\001\255\255\255\255\
\255\255\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\018\001\019\001\255\255\255\255\022\001\
\255\255\024\001\255\255\255\255\255\255\028\001\255\255\030\001\
\255\255\255\255\255\255\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\255\255\018\001\019\001\255\255\
\255\255\022\001\255\255\024\001\255\255\255\255\255\255\028\001\
\255\255\030\001\255\255\255\255\255\255\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\019\001\255\255\255\255\022\001\255\255\024\001\018\001\019\001\
\255\255\028\001\022\001\030\001\024\001\255\255\255\255\255\255\
\028\001\255\255\030\001\255\255\255\255\255\255\255\255\255\255\
\255\255\018\001\019\001\255\255\255\255\022\001\255\255\024\001\
\255\255\255\255\255\255\028\001\255\255\030\001\255\255\255\255\
\255\255\255\255\255\255\018\001\019\001\255\255\255\255\022\001\
\255\255\255\255\255\255\255\255\255\255\028\001\255\255\030\001\
\255\255\018\001\019\001\018\001\019\001\022\001\255\255\022\001\
\255\255\255\255\255\255\028\001\255\255\030\001\255\255\030\001\
\255\255\255\255\255\255\255\255\255\255\018\001\019\001\255\255\
\255\255\022\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\030\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\255\255\
\019\001\255\255\255\255\255\255\255\255\024\001\255\255\255\255\
\255\255\028\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\255\255\
\255\255\255\255\255\255\022\001\255\255\024\001\255\255\255\255\
\255\255\028\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\255\255\
\255\255\255\255\255\255\022\001\255\255\024\001\255\255\255\255\
\255\255\028\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\024\001\255\255\255\255\
\255\255\028\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\024\001"

let yynames_const = "\
  NOT\000\
  MINUS\000\
  PLUS\000\
  MINUS_DOT\000\
  PLUS_DOT\000\
  AST_DOT\000\
  SLASH_DOT\000\
  EQUAL\000\
  LESS_GREATER\000\
  LESS_EQUAL\000\
  GREATER_EQUAL\000\
  LESS\000\
  GREATER\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LET\000\
  IN\000\
  REC\000\
  COMMA\000\
  ARRAY_CREATE\000\
  DOT\000\
  LESS_MINUS\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL\000\
  INT\000\
  FLOAT\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Syntax.t) in
    Obj.repr(
# 64 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                   ( _2          )
# 426 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'simple_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                   ( Unit        )
# 432 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'simple_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 66 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                   ( Bool(_1)    )
# 439 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'simple_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                   ( Int(_1)     )
# 446 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'simple_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 68 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                   ( Float(_1)   )
# 453 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'simple_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Id.t) in
    Obj.repr(
# 69 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                   ( Var(_1)     )
# 460 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'simple_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'simple_exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Syntax.t) in
    Obj.repr(
# 70 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                   ( Get(_1, _4) )
# 468 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'simple_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_exp) in
    Obj.repr(
# 73 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
             ( _1 )
# 475 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 76 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                       ( Not(_2)  )
# 482 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 77 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                       ( FNeg(_2) )
# 489 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 78 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                       ( match _2 with
                                         (* -1.23などは型エラーではないので別扱い *)
                                         | Float(f) -> Float(-.f)
                                         | e -> Neg(e) )
# 499 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 84 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( Add (_1, _3)    )
# 507 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 85 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( Sub (_1, _3)    )
# 515 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 86 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( FAdd(_1, _3)    )
# 523 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 87 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( FSub(_1, _3)    )
# 531 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 88 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( FMul(_1, _3)    )
# 539 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 89 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( FDiv(_1, _3)    )
# 547 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 90 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( Eq  (_1, _3)    )
# 555 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 91 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( LE  (_1, _3)    )
# 563 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 92 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( LE  (_3, _1)    )
# 571 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 93 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( Not(Eq(_1, _3)) )
# 579 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 94 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( Not(LE(_3, _1)) )
# 587 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 95 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                        ( Not(LE(_1, _3)) )
# 595 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'fundef) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 98 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                                        ( LetRec(_3, _5)         )
# 603 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Id.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 99 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                                        ( Let(addtyp _2, _4, _6) )
# 612 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'pat) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 100 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                                        ( LetTuple(_3, _6, _8)   )
# 621 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 103 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                                      ( Let((Id.gentmp Type.Unit, Type.Unit), _1, _3) )
# 629 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 104 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                                      ( Tuple(_1)                                     )
# 636 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Syntax.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 105 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                                      ( If(_2, _4, _6)                                )
# 645 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'actual_args) in
    Obj.repr(
# 106 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                                      ( App(_1, _2)                                   )
# 653 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'simple_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple_exp) in
    Obj.repr(
# 107 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                                      ( Array(_2, _3)                                 )
# 661 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'simple_exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Syntax.t) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 108 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                                      ( Put(_1, _4, _7)                               )
# 670 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
           ( (* エラーの出力 *)
      raise (Error.ParseError (symbol_start_pos (), symbol_end_pos ())) )
# 677 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : Syntax.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Id.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'formal_args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 116 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                 ( { name = addtyp _1; args = _2; body = _4 } )
# 686 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'fundef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Id.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formal_args) in
    Obj.repr(
# 120 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                    ( addtyp _1 :: _2 )
# 694 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'formal_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Id.t) in
    Obj.repr(
# 121 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                    ( [addtyp _1]     )
# 701 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'formal_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'actual_args) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simple_exp) in
    Obj.repr(
# 124 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                        ( _1 @ [_2] )
# 709 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'actual_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_exp) in
    Obj.repr(
# 125 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                                        ( [_1]      )
# 716 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'actual_args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tuple) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 128 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                  ( _1 @ [_3] )
# 724 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Syntax.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t) in
    Obj.repr(
# 129 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                  ( [_1; _3]  )
# 732 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Id.t) in
    Obj.repr(
# 132 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                    ( _1 @ [addtyp _3]       )
# 740 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'pat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Id.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Id.t) in
    Obj.repr(
# 133 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser"
                    ( [addtyp _1; addtyp _3] )
# 748 "/home/yuki/Documents/try-min-caml-online/src/min-caml/parser.ml"
               : 'pat))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.t)
