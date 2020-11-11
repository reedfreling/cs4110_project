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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
open Printf
open Lexing

let merge (fn,pos1,_) (_,_,pos2) = (fn,pos1,pos2)
# 29 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* TRUE *);
  261 (* FALSE *);
  262 (* NOT *);
  263 (* AND *);
  264 (* OR *);
  265 (* LBRACE *);
  266 (* RBRACE *);
  267 (* IMPLIES *);
  268 (* IFF *);
  269 (* ASSIGN *);
  270 (* SEMI *);
  271 (* PRINT *);
  272 (* INTRO *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\004\000\004\000\005\000\005\000\006\000\
\006\000\007\000\007\000\007\000\007\000\002\000\002\000\008\000\
\008\000\008\000\008\000\003\000\000\000"

let yylen = "\002\000\
\003\000\003\000\001\000\003\000\001\000\003\000\001\000\002\000\
\001\000\001\000\001\000\003\000\001\000\003\000\001\000\003\000\
\002\000\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\
\000\000\000\000\000\000\013\000\000\000\010\000\011\000\000\000\
\000\000\000\000\000\000\007\000\009\000\017\000\020\000\000\000\
\000\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\012\000\000\000\000\000\000\000\006\000"

let yydgoto = "\002\000\
\017\000\007\000\008\000\018\000\019\000\020\000\021\000\009\000"

let yysindex = "\004\000\
\013\255\000\000\251\254\013\255\031\255\012\255\015\000\000\000\
\005\255\031\255\015\255\000\000\031\255\000\000\000\000\031\255\
\247\254\019\255\023\255\000\000\000\000\000\000\000\000\013\255\
\247\254\000\000\006\255\247\254\031\255\031\255\031\255\031\255\
\000\000\000\000\247\254\247\254\023\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\001\000\006\000\000\000\000\000\000\000\000\000\000\000\
\021\000\000\000\000\000\026\000\000\000\000\000\000\000\000\000\
\000\000\000\000\034\000\042\000\016\000\000\000"

let yygindex = "\000\000\
\250\255\252\255\000\000\000\000\007\000\255\255\000\000\000\000"

let yytablesize = 312
let yytable = "\011\000\
\003\000\029\000\030\000\025\000\001\000\005\000\027\000\010\000\
\034\000\028\000\019\000\015\000\022\000\003\000\023\000\004\000\
\029\000\030\000\024\000\033\000\016\000\004\000\035\000\036\000\
\026\000\008\000\031\000\005\000\006\000\032\000\038\000\012\000\
\013\000\001\000\014\000\015\000\016\000\037\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\000\000\000\000\000\000\003\000\
\005\000\000\000\003\000\003\000\003\000\005\000\003\000\005\000\
\005\000\005\000\004\000\005\000\019\000\015\000\000\000\004\000\
\019\000\004\000\004\000\004\000\008\000\004\000\016\000\000\000\
\008\000\008\000\016\000\008\000\001\000\000\000\000\000\008\000\
\001\000\001\000\000\000\001\000\002\000\000\000\000\000\001\000\
\002\000\002\000\000\000\002\000\000\000\000\000\000\000\002\000"

let yycheck = "\004\000\
\000\000\011\001\012\001\010\000\001\000\000\000\013\000\013\001\
\003\001\016\000\000\000\000\000\001\001\001\001\000\000\000\000\
\011\001\012\001\014\001\024\000\000\000\009\001\029\000\030\000\
\010\001\000\000\008\001\015\001\016\001\007\001\032\000\001\001\
\002\001\000\000\004\001\005\001\006\001\031\000\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\255\255\255\255\007\001\
\003\001\255\255\010\001\011\001\012\001\008\001\014\001\010\001\
\011\001\012\001\003\001\014\001\010\001\010\001\255\255\008\001\
\014\001\010\001\011\001\012\001\003\001\014\001\010\001\255\255\
\007\001\008\001\014\001\010\001\003\001\255\255\255\255\014\001\
\007\001\008\001\255\255\010\001\003\001\255\255\255\255\014\001\
\007\001\008\001\255\255\010\001\255\255\255\255\255\255\014\001"

let yynames_const = "\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  LPAREN\000\
  RPAREN\000\
  TRUE\000\
  FALSE\000\
  NOT\000\
  AND\000\
  OR\000\
  LBRACE\000\
  RBRACE\000\
  IMPLIES\000\
  IFF\000\
  ASSIGN\000\
  SEMI\000\
  PRINT\000\
  INTRO\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.bexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.bexp) in
    Obj.repr(
# 27 "parser.mly"
                          ( Implies(_1, _3) )
# 205 "parser.ml"
               : Ast.bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.bexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.bexp) in
    Obj.repr(
# 28 "parser.mly"
                          ( Iff (_1, _3) )
# 214 "parser.ml"
               : Ast.bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'db) in
    Obj.repr(
# 29 "parser.mly"
                          ( _1 )
# 221 "parser.ml"
               : Ast.bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'db) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cb) in
    Obj.repr(
# 31 "parser.mly"
                          ( Or(_1, _3) )
# 230 "parser.ml"
               : 'db))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cb) in
    Obj.repr(
# 32 "parser.mly"
                          ( _1 )
# 237 "parser.ml"
               : 'db))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cb) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nb) in
    Obj.repr(
# 34 "parser.mly"
                          ( And(_1, _3) )
# 246 "parser.ml"
               : 'cb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nb) in
    Obj.repr(
# 35 "parser.mly"
                          ( _1 )
# 253 "parser.ml"
               : 'cb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.bexp) in
    Obj.repr(
# 37 "parser.mly"
                          ( Not(_2) )
# 261 "parser.ml"
               : 'nb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ab) in
    Obj.repr(
# 38 "parser.mly"
                          ( _1 )
# 268 "parser.ml"
               : 'nb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.info) in
    Obj.repr(
# 40 "parser.mly"
                          ( True )
# 275 "parser.ml"
               : 'ab))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.info) in
    Obj.repr(
# 41 "parser.mly"
                          ( False )
# 282 "parser.ml"
               : 'ab))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.info) in
    Obj.repr(
# 42 "parser.mly"
                          ( _2 )
# 291 "parser.ml"
               : 'ab))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.info * string) in
    Obj.repr(
# 43 "parser.mly"
                          ( Var(snd _1) )
# 298 "parser.ml"
               : 'ab))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ac) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.com) in
    Obj.repr(
# 46 "parser.mly"
                          ( Seq(_1, _3) )
# 307 "parser.ml"
               : Ast.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ac) in
    Obj.repr(
# 47 "parser.mly"
                          ( _1 )
# 314 "parser.ml"
               : Ast.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.info * string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.bexp) in
    Obj.repr(
# 49 "parser.mly"
                          ( Assign(snd _1, _3) )
# 323 "parser.ml"
               : 'ac))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.info * string) in
    Obj.repr(
# 50 "parser.mly"
                          ( Intro (snd _2) )
# 331 "parser.ml"
               : 'ac))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.com) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.info) in
    Obj.repr(
# 51 "parser.mly"
                          ( _2 )
# 340 "parser.ml"
               : 'ac))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.bexp) in
    Obj.repr(
# 52 "parser.mly"
                          ( Print _2 )
# 348 "parser.ml"
               : 'ac))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.com) in
    Obj.repr(
# 55 "parser.mly"
                          ( _1 )
# 355 "parser.ml"
               : Ast.com))
(* Entry p *)
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
let p (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.com)
