{
open Parser
open Printf
exception Eof
exception LexingError of string

let lineno = ref 1
let linestart = ref (-1)

let newline lexbuf : unit =
  linestart := Lexing.lexeme_start lexbuf;
  incr lineno

let info lexbuf =
  let c1 = Lexing.lexeme_start lexbuf in
  let c2 = Lexing.lexeme_end lexbuf in
  let l = !lineno in
  let c = !linestart + 1 in
    ((l, c1 - c),(l, c2 - c - 1))

let error lexbuf msg =
  let i = info lexbuf in
  let t = Lexing.lexeme lexbuf in
  let ((l1,c1),(l2,c2)) = i in
  let s =
    if l2=l1
    then Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
    else Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2 in
  let err = Printf.sprintf "%s: lexing error %s at %s."
    s
    msg
    t in
  raise (LexingError err)
}

let digit = ['-']?['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let ws = [' ' '\t']
let s = ['{'] ([' ']* id [' ']* [','])* [' ']* id [' ']* ['}']
let pset = ['{'] ([' ']* ['('] [' ']* id [' ']* [','] [' ']* id [' ']* [')'] [' ']* [';'])* [' ']* ['('] [' ']* id [' ']* [','] [' ']* id [' ']* [')'] [' ']* ['}']

rule token = parse
| ws      { token lexbuf }
| '\n'    { newline lexbuf; token lexbuf }
| "("     { LPAREN(info lexbuf) }
| ")"     { RPAREN(info lexbuf) }
| s as l  { SET(info lexbuf, l) }
| pset as l { PAIRSET(info lexbuf, l) }
| "{"     { LBRACE(info lexbuf) }
| "}"     { RBRACE(info lexbuf) }
| ";"     { SEMI(info lexbuf) }
| ":="    { ASSIGN(info lexbuf) }
| "true"  { TRUE(info lexbuf) }
| "false" { FALSE(info lexbuf) }
| "!"     { NOT(info lexbuf) }
| "and"   { AND(info lexbuf) }
| "or"    { OR(info lexbuf) }
| "->"    { IMPLIES(info lexbuf) }
| "<->"   { IFF(info lexbuf) }
| "print" { PRINT(info lexbuf) }
| "intros"{ INTROS(info lexbuf) }
| "intro" { INTRO(info lexbuf) }
| "[]"    { SQUARE(info lexbuf) } 
| "<>"    { DIAMOND(info lexbuf) }
| "||-"   { GETTRUTH(info lexbuf) }
| "create" { CREATEKRIPKE(info lexbuf) }
| "add world" { ADDWORLD(info lexbuf) }
| "add worlds" { ADDWORLDS(info lexbuf) }
| "add accessibility" { ADDACCESS(info lexbuf) }
| "add accessibilities" { ADDACCESSES(info lexbuf) }
| "add valuation" { ADDVALUE(info lexbuf) }
| "add valuations" { ADDVALUES(info lexbuf) }
| id as v { VAR(info lexbuf, v) }
| eof     { EOF }
| _ as c  { error lexbuf (String.make 1 c) }