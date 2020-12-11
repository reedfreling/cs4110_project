open Ast

let sp = Printf.sprintf

let rec pprintBexp b = print_string(strBexp b)
and pprintCom c = print_string(strCom(0, c))
and pprintInfo i = print_string(strInfo i)
and space n = if n = 0 then "" else " " ^ space(n-1)
and strBexp e = match e with
  | True -> "true"
  | False -> "false"
  | Implies(b1, b2) -> sp "(%s -> %s)" (strBexp b1) (strBexp b2)
  | Iff(b1, b2) -> sp "(%s <-> %s)" (strBexp b1) (strBexp b2)
  | Not(b) -> sp "not(%s)" (strBexp b)
  | And(b1, b2) -> sp "(%s and %s)" (strBexp b1) (strBexp b2)
  | Or(b1, b2) -> sp "(%s or %s)" (strBexp b1) (strBexp b2)
  | Var x -> x
  | Unknown (x, b) -> strBexp b
  | _ -> failwith "printing modal expressions not supported right now"
and strCom(n, c) =
  match c with
  | Assign(x, b) -> sp "%s%s := %s" (space n) x (strBexp b)
  | Seq(c1, c2) -> sp "%s;\n%s" (strCom(n, c1)) (strCom(n, c2))
  | Print b -> sp "%sprint %s" (space(n)) (strBexp b)
  | Intro x -> sp "%sprint %s" (space(n)) (strBexp (Var x))
  | _ -> failwith "No printing for this"
and strInfo ((l1,c1),(l2,c2)) =
  if l2=l1
  then Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
  else Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2
