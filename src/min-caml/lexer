{
(* lexerが利用する変数、関数、型などの定義 *)
open Parser
open Lexing
open Type
}

(* 正規表現の略記 *)
let space = [' ' '\t' '\r'] (* \n を除く *)
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let num_s = digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
let arr_m = "Array.create" | "Array.make"
let ident = lower (digit|lower|upper|'_')*

rule token = parse
(* 行番号を取得できるようにLexing.new_lineを呼ぶ *)
| '\n'    { Lexing.new_line lexbuf; token lexbuf          }
| space+  { token lexbuf                                  }
| "(*"    { comment lexbuf; token lexbuf                  } (* ネストしたコメントのためのトリック *)
| '('     { LPAREN                                        }
| ')'     { RPAREN                                        }
| "true"  { BOOL(true)                                    }
| "false" { BOOL(false)                                   }
| "not"   { NOT                                           }
| digit+  { INT(int_of_string (Lexing.lexeme lexbuf))     } (* 整数を字句解析するルール (caml2html: lexer_int) *)
| num_s   { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '-'     { MINUS                                         } (* -.より後回しにしなくても良い? 最長一致? *)
| '+'     { PLUS                                          } (* +.より後回しにしなくても良い? 最長一致? *)
| "-."    { MINUS_DOT                                     }
| "+."    { PLUS_DOT                                      }
| "*."    { AST_DOT                                       }
| "/."    { SLASH_DOT                                     }
| '='     { EQUAL                                         }
| "<>"    { LESS_GREATER                                  }
| "<="    { LESS_EQUAL                                    }
| ">="    { GREATER_EQUAL                                 }
| '<'     { LESS                                          }
| '>'     { GREATER                                       }
| "if"    { IF                                            }
| "then"  { THEN                                          }
| "else"  { ELSE                                          }
| "let"   { LET                                           }
| "in"    { IN                                            }
| "rec"   { REC                                           }
| ','     { COMMA                                         }
| '_'     { IDENT(Id.gentmp Type.Unit)                    }
| arr_m   { ARRAY_CREATE                                  } (* [XX] ad hoc *)
| '.'     { DOT                                           }
| "<-"    { LESS_MINUS                                    }
| ';'     { SEMICOLON                                     }
| eof     { EOF                                           }
| ident   { IDENT(Lexing.lexeme lexbuf)                   } (* 他の「予約語」より後でないといけない *)
| _    (* エラーを出力する関数を呼ぶ *)
    { Error.explain_lex_error lexbuf
        (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)     }

and comment = parse
(* 行番号を取得できるようにLexing.new_lineを呼ぶ *)
| '\n'    { Lexing.new_line lexbuf; comment lexbuf        }
| "*)"    { ()                                            }
| "(*"    { comment lexbuf; comment lexbuf                }
| eof     { Format.eprintf "warning: unterminated comment"}
| _       { comment lexbuf                                }
