%{
open Ast
open Printf
open Lexing

let merge (fn,pos1,_) (_,_,pos2) = (fn,pos1,pos2)
%}

%token <Ast.info * string> VAR WORLDS
%token <Ast.info>
  LPAREN RPAREN TRUE FALSE
  NOT AND OR
  LBRACE RBRACE
  IMPLIES IFF 
  ASSIGN SEMI PRINT
  INTRO 
  GETTRUTH CREATEKRIPKE ADDWORLD ADDACCESS ADDVALUE ADDWORLDS
  BEXP SQUARE DIAMOND
%token EOF

%type <Ast.bexp> b
%type <Ast.com> c
%type <Ast.mexp> m
%type <Ast.kripke_bexp> kb
%type <Ast.com> p

%start p

%%
/* Boolean Expressions */
b : b IMPLIES b           { Implies($1, $3) }
  | b IFF b               { Iff ($1, $3) }
  | db                    { $1 }

db: db OR cb              { Or($1, $3) }
  | cb                    { $1 }

cb: cb AND nb             { And($1, $3) }
  | nb                    { $1 }

nb: NOT b                 { Not($2) }
  | ab                    { $1 }

ab : TRUE                 { True }
   | FALSE                { False }
   | LPAREN b RPAREN      { $2 }
   | VAR                  { Var(snd $1) }

/* Commands */
c : ac SEMI c             { Seq($1, $3) }
  | ac                    { $1 }

ac: VAR ASSIGN b          { Assign(snd $1, $3) }
  | VAR ASSIGN kb         { AssignMexp(snd $1, $3) }
  | INTRO VAR             { Intro (snd $2) }
  | LBRACE c RBRACE       { $2 }
  | PRINT b               { Print $2 }
  | kc                    { $1 }

/* kripke boolean expressions */
kb : VAR VAR GETTRUTH m { GetTruthValueFromKripke(snd $1, (snd $2, $4)) }

/* Modal logic expressions */
m : SQUARE m              { Square($2) }
  | DIAMOND m             { Diamond($2) }
  | b                     { Bexp($1) }

/* Kripke Commands */
kc : CREATEKRIPKE VAR     { CreateEmptyKripke(snd $2) }
  | VAR ADDWORLD VAR      { AddWorldToKripke(snd $1, snd $3) }
  | VAR ADDACCESS VAR VAR { AddAccessToKripke(snd $1, (snd $3, snd $4)) }
  | VAR ADDVALUE VAR VAR  { AddValuationToKripke(snd $1, (snd $4, snd $3)) }
  | VAR ADDWORLDS WORLDS  { AddWorldsToKripke(snd $1, snd $3) }

/* Programs */
p : c EOF                 { $1 }
