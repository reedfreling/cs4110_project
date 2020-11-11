type token =
  | VAR of (Ast.info * string)
  | LPAREN of (Ast.info)
  | RPAREN of (Ast.info)
  | TRUE of (Ast.info)
  | FALSE of (Ast.info)
  | NOT of (Ast.info)
  | AND of (Ast.info)
  | OR of (Ast.info)
  | LBRACE of (Ast.info)
  | RBRACE of (Ast.info)
  | IMPLIES of (Ast.info)
  | IFF of (Ast.info)
  | ASSIGN of (Ast.info)
  | SEMI of (Ast.info)
  | PRINT of (Ast.info)
  | INTRO of (Ast.info)
  | EOF

val p :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.com
