open Ast

(* (w1, w2) in W <=> w2 is accessible from w1 *)
type access_relation = (world * world) list

(* if a world appears in the list associated to some propositional var
  we consider that variable to be true in that world
 *)
type valuation_function = (var * world list) list

type kripke = (world list * access_relation * valuation_function)

let list_union l1 l2 = List.fold_left (fun acc elt -> if List.mem elt acc then acc else elt::acc) l1 l2

(* start off with empty model *)
let empty = ([], [], [])

let add_worlds (k : kripke) (w : world list) =
  match k with
  | (worlds, r, v) -> ((list_union w worlds), r, v)

let add_accessibility (k : kripke) (world_pair : world * world) =
  match k with
  | (w, r, v) -> (w, world_pair::r, v)

let add_accessibilities (k : kripke) (world_pairs : (world * world) list) =
  List.fold_left (fun acc elt -> add_accessibility acc elt) k world_pairs 

let add_valuation (k : kripke) (x : var) (w : world) =
  let w_set = [w] in
  match k with
  | (worlds, r, v) -> 
    let current_valuation = List.assoc_opt x v in
    (
      match current_valuation with
      | None -> (worlds, r, (x, w_set)::v)
      | Some curr_val -> (worlds, r, (x, list_union w_set curr_val)::(List.remove_assoc x v))
    )

let add_valuations (k : kripke) (x : var list) (w : world) =
  List.fold_left (fun acc elt -> add_valuation acc elt w) k x

(* check if w2 is accessible from w1 *)
let rec check_accessibility (m : kripke) (w1 : world) (w2 : world) = 
  match m with
  | (worlds, r, v) -> let w = List.assoc_opt w1 r in
    (
      match w with
      | None -> false
      | Some w'-> if w2 = w' then true else 
        let r' = List.remove_assoc w1 r in 
        let m' = (worlds, r', v) in
        check_accessibility m' w1 w2
    )

(* return the set of worlds in m that are accessible from w *)
let rec accessible_worlds (m : kripke) (w : world) = 
  match m with
  | (w'::worlds, r, v) -> let m' = (worlds, r, v) in
    if check_accessibility m w w' then w'::(accessible_worlds m' w)
    else (accessible_worlds m' w)
  | ([], r, v) -> []

let rec eval_bexp (m : kripke) (w : world) (e : bexp) : bexp = 
  match e with
  | True -> True
  | False -> False
  | Implies (e1, e2) -> eval_bexp m w (Or (Not e1, e2))
  | Iff (e1, e2) -> eval_bexp m w (And (Implies(e1, e2), Implies(e2, e1)))
  | Not True -> False
  | Not False -> True
  | Not e' -> Not (eval_bexp m w e')
  | And (True, e') -> eval_bexp m w e'
  | And (False, _) -> False
  | And (e1, e2) -> eval_bexp m w (And (eval_bexp m w e1, e2))
  | Or (True, _) -> True
  | Or (False, e') -> eval_bexp m w e'
  | Or (e1, e2) -> eval_bexp m w (Or (eval_bexp m w e1, e2))
  | Var x -> 
    (
    match m with
    | (worlds, r, v) -> 
      (
      match List.assoc_opt x v with
      | Some wlist -> if List.mem w wlist then True else False
      | None -> False
      )
    )
  | Unknown (v, e') -> failwith "should be no unknowns"

let rec check_valuation_at_world (m : kripke) (w : world) (e : mexp) : bexp = 
  match e with
  | Bexp e' -> 
    eval_bexp m w e'
  | Square e' -> 
    let a_worlds = accessible_worlds m w in
    true_at_all m a_worlds e'
  | Diamond e' -> 
    let a_worlds = accessible_worlds m w in
    true_at_some m a_worlds e'
and true_at_all (m : kripke) (worlds : world list) (e : mexp) =
  match e with
  | Bexp e' -> 
    (
      match worlds with
      | w'::worlds' -> And ((check_valuation_at_world m w' e), (true_at_all m worlds' e))
      | [] -> True
    )
  | Square e' -> 
    (
      match worlds with
      | w'::worlds' -> 
        let a_worlds = accessible_worlds m w' in
        And ((true_at_all m a_worlds e'), (true_at_all m worlds' e'))
      | [] -> True
    )
  | Diamond e' -> 
    (
      match worlds with
      | w'::worlds' -> 
        let a_worlds = accessible_worlds m w' in
        And ((true_at_some m a_worlds e'), (true_at_all m worlds' e'))
      | [] -> True
    )
and true_at_some (m : kripke) (worlds : world list) (e : mexp) = 
  match e with
  | Bexp e' -> 
    (
      match worlds with
      | w'::worlds' -> Or ((check_valuation_at_world m w' e), (true_at_some m worlds' e))
      | [] -> False
    )
  | Square e' -> 
    (
      match worlds with
      | w'::worlds' -> 
        let a_worlds = accessible_worlds m w' in
        Or ((true_at_all m a_worlds e'), (true_at_some m worlds' e'))
      | [] -> False
    )
  | Diamond e' -> 
    (
      match worlds with
      | w'::worlds' -> 
        let a_worlds = accessible_worlds m w' in
        Or ((true_at_some m a_worlds e'), (true_at_some m worlds' e'))
      | [] -> False
    )

let eval_mexp (m : kripke) (w : world) (e : mexp) : bexp = 
  match m with
  | (worlds, r, v) ->
    let b = eval_bexp m w (check_valuation_at_world m w e) in
    match b with
    | True -> True
    | False -> False
    | _ -> failwith "e could not be evaluated to T/F"

let produce_latex_node (world : world) (pos : int * int) =
  String.concat "" ["\\node[world] "; "("; world; ") ["; 
    "xshift="; string_of_int (fst pos); "cm,"; 
    "yshift="; string_of_int (snd pos); "cm] ";
    "{$"; world; "$};"]

let produce_position (i : int) =
  if i mod 2 = 0 then (0, -(i/2)*5) else (5, -(i/2)*5)

let produce_latex_nodes (worlds : world list) =
  String.concat "\n" (List.mapi (fun i elt -> produce_latex_node elt (produce_position i)) worlds)

let produce_latex_edge (edge : world * world) =
  match edge with
  | w1, w2 ->
    String.concat "" ["\\path[->]("; w1; ") "; "edge[bend left] ("; w2; ");"]

let produce_latex_edges (r : access_relation) = 
  List.fold_right (fun elt acc -> String.concat "\n" [acc; produce_latex_edge elt]) r ""

let produce_latex_graph (m : kripke) =
  match m with
  | (worlds, r, v) -> String.concat "\n" [produce_latex_nodes worlds; produce_latex_edges r]

let rec produce_repeated_list s i =
  if i = 0 then [] else s::(produce_repeated_list s (i - 1))

let valuation_truth_table_tabular i =
  let begin_tabular = "\\begin{tabular}{ " in
  let columns = produce_repeated_list "|c" (i + 1) in
  String.concat "" [begin_tabular; String.concat "" columns; "| }"]

let truth_table_variables (v : valuation_function) =
  List.map (fun (k, v) -> k) v

let valuation_truth_table_first_row (worlds : world list) = 
  String.concat " & " (" "::worlds)

let valuation_truth_table_row (worlds : world list) (v : valuation_function) (p : var) =
  let p_worlds = List.assoc p v in
  let valuations = List.map (fun w -> if List.mem w p_worlds then "t" else "f") worlds in
  String.concat " & " (p::valuations)

let valuation_truth_table_rows (worlds : world list) (v : valuation_function) =
  let string_rows =
    List.map (fun p -> valuation_truth_table_row worlds v p) (truth_table_variables v) in
  String.concat " \\\\ \n" string_rows

let valuation_truth_table (m : kripke) =
  match m with
  | (w, r, v) ->
    String.concat "\n\n"
    [
      "\\begin{center}";
      valuation_truth_table_tabular (List.length w); "\\hline";
      String.concat "" (valuation_truth_table_first_row w::[" \\\\"]); "\\hline"; 
      valuation_truth_table_rows w v; "\\\\ \\hline";
      "\\end{tabular}";
      "\\end{center}"
    ]

let produce_latex_document (m : kripke) =
  String.concat "\n\n"
 ["\\documentclass[12pt]{article}"; "\\usepackage{tikz}"; 
  "\\usetikzlibrary{positioning,arrows,calc}";
  "\\tikzset{
  modal/.style={>=stealth,shorten >=1pt,shorten <=1pt,auto,node distance=1.5cm,
  semithick},
  world/.style={circle,draw,minimum size=0.5cm,fill=gray!15},
  point/.style={circle,draw,inner sep=0.5mm,fill=black},
  reflexive above/.style={->,loop,looseness=7,in=120,out=60},
  reflexive below/.style={->,loop,looseness=7,in=240,out=300},
  reflexive left/.style={->,loop,looseness=7,in=150,out=210},
  reflexive right/.style={->,loop,looseness=7,in=30,out=330}
}";
  "\\begin{document}";
  "\\begin{center}"; "\\begin{tikzpicture}[modal]"; produce_latex_graph m; 
  "\\end{tikzpicture}"; "\\end{center}";
  valuation_truth_table m;
  "\\end{document}"]
  

let latex_kripke (m : kripke) (file : string) = 
  let oc = open_out file in
  Printf.fprintf oc "%s" (produce_latex_document m);
  close_out oc 
 
  