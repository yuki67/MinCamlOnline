%{
(* parserが利用する変数、関数、型などの定義 *)
open Syntax
(* 「式xの型は未確定である」を表す組を返す*)
let addtyp x = (x, Type.gentyp ())
%}

/* (* 字句を表すデータ型の定義 (caml2html: parser_token) *) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF

/* (* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) *) */
/* トークンでない変数(prec_*)は規則の中で一時的に結合の強さを変化させるために使われる(規則の"%prec ~") */
%nonassoc IN
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%nonassoc prec_tuple
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT

/* (* 開始記号の定義 *) */
%type <Syntax.t> exp
%start exp

%%

simple_exp: /* (* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) *) */
| LPAREN exp RPAREN                { $2          }
| LPAREN RPAREN                    { Unit        }
| BOOL                             { Bool($1)    }
| INT                              { Int($1)     }
| FLOAT                            { Float($1)   }
| IDENT                            { Var($1)     }
| simple_exp DOT LPAREN exp RPAREN { Get($1, $4) }

exp: /* (* 一般の式 (caml2html: parser_exp) *) */
| simple_exp { $1 }

/* 一項演算子 */
| NOT       exp %prec prec_app         { Not($2)  }
| MINUS_DOT exp %prec prec_unary_minus { FNeg($2) }
| MINUS     exp %prec prec_unary_minus { match $2 with
                                         (* -1.23などは型エラーではないので別扱い *)
                                         | Float(f) -> Float(-.f)
                                         | e -> Neg(e) }

/* 二項演算子 */
| exp PLUS          exp { Add ($1, $3)    } /* (* 足し算を構文解析するルール (caml2html: parser_add) *) */
| exp MINUS         exp { Sub ($1, $3)    }
| exp PLUS_DOT      exp { FAdd($1, $3)    }
| exp MINUS_DOT     exp { FSub($1, $3)    }
| exp AST_DOT       exp { FMul($1, $3)    }
| exp SLASH_DOT     exp { FDiv($1, $3)    }
| exp EQUAL         exp { Eq  ($1, $3)    }
| exp LESS_EQUAL    exp { LE  ($1, $3)    }
| exp GREATER_EQUAL exp { LE  ($3, $1)    } /* 不等号はパース時にLEを使ったものにに変換する */
| exp LESS_GREATER  exp { Not(Eq($1, $3)) }
| exp LESS          exp { Not(LE($3, $1)) }
| exp GREATER       exp { Not(LE($1, $3)) }

/* let式 */
| LET REC fundef                  IN exp %prec prec_let { LetRec($3, $5)         }
| LET IDENT             EQUAL exp IN exp %prec prec_let { Let(addtyp $2, $4, $6) }
| LET LPAREN pat RPAREN EQUAL exp IN exp                { LetTuple($3, $6, $8)   }

/* その他 */
| exp SEMICOLON exp                                   { Let((Id.gentmp Type.Unit, Type.Unit), $1, $3) } /* 複文             */
| tuple                              %prec prec_tuple { Tuple($1)                                     } /* タプル           */
| IF exp THEN exp ELSE exp           %prec prec_if    { If($2, $4, $6)                                } /* if式             */
| simple_exp actual_args             %prec prec_app   { App($1, $2)                                   } /* 関数適用         */
| ARRAY_CREATE simple_exp simple_exp %prec prec_app   { Array($2, $3)                                 } /* 配列の作成       */
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp     { Put($1, $4, $7)                               } /* 配列への書き込み */

/* その他: エラー */
| error    { (* エラーの出力 *)
      raise (Error.ParseError (symbol_start_pos (), symbol_end_pos ())) }

/* 関数の定義 */
fundef:
| IDENT formal_args EQUAL exp    { { name = addtyp $1; args = $2; body = $4 } }

/* 何かの並び */
formal_args: /* 関数定義の引数: 変数と型変数の組にする必要がある */
| IDENT formal_args { addtyp $1 :: $2 }
| IDENT             { [addtyp $1]     }

actual_args: /* 関数適用の引数: ただの式の並び */
| actual_args simple_exp %prec prec_app { $1 @ [$2] }
| simple_exp             %prec prec_app { [$1]      }

tuple:
| tuple COMMA exp { $1 @ [$3] }
| exp   COMMA exp { [$1; $3]  }

pat:
| pat   COMMA IDENT { $1 @ [addtyp $3]       }
| IDENT COMMA IDENT { [addtyp $1; addtyp $3] }
