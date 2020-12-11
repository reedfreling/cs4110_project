open Ast
open Pprint
open Kripke

(* Interpreter exceptions. *)
exception UnboundVariable of var
exception UnboundedKripkeModel of var
exception AssignVarToSelf of var

(* A type for stores. *)
type store = (var * bexp) list

type var_store = var list

type kripke_store = (var * kripke) list

(* A type for configurations. *)
(* sigma, c, c', k *)
type configuration = (store * var_store * kripke_store * com)

let print_bexp b =
  print_string (strBexp b)

(* Create an initial configuration from a command. *)
let make_configuration (c:com) : configuration =
  ([], [], [], c)

(* Evaluate a boolean expression *)
let rec beval (sigma : store) (var_sigma : var_store) (b : bexp) : bexp =
  match b with
  | True -> True
  | False -> False
  | Not True -> False
  | Not False -> True
  | Not (Unknown (x, b)) -> Unknown (x, Not b)
  | Not b -> Not (beval sigma var_sigma b)
  | And (True, b) -> beval sigma var_sigma b
  | And (False, _) -> False
  | And (Unknown (x, b1), b2) -> Unknown (x, And (b1, beval sigma var_sigma b2))
  | And (b1, b2) -> beval sigma var_sigma (And (beval sigma var_sigma b1, b2))
  | Or (True, _) -> True
  | Or (False, b) -> beval sigma var_sigma b
  | Or (Unknown (x, b1), b2) -> Unknown (x, Or (b1, beval sigma var_sigma b2))
  | Or (b1, b2) -> beval sigma var_sigma (Or (beval sigma var_sigma b1, b2))
  | Implies (b1, b2) -> beval sigma var_sigma (Or (Not b1, b2))
  | Iff (b1, b2) -> beval sigma var_sigma (And (Implies(b1, b2), Implies(b2, b1)))
  | Var x ->  (
    match List.assoc_opt x sigma with
    | Some b -> beval sigma var_sigma b
    | None -> (
      if List.mem x var_sigma then Unknown (x, Var x) else raise (UnboundVariable x)
    )
  )
  | Unknown (x, b) -> 
    if List.exists (fun (x', b') -> x' = x) sigma then beval sigma var_sigma b else Unknown (x, b)

let print_list l = 
  print_string "["; List.iter (fun x -> print_string x; print_string "; ") l; print_string "]"

let parse_set (s : string) =
  let variables = String.trim s in
  let variables = String.map (fun c -> if c = '{' || c = '}' then ',' else c) variables in
  let variables = String.split_on_char ',' variables in
  let variables = List.map (String.trim) variables in
  let variables = List.filter (fun x -> not (String.length x = 0)) variables in
    variables

let parse_pair p = 
    let pair = String.map (fun c -> if c = '(' || c = ')' then ',' else c) p in
    let pair = String.split_on_char ',' pair in 
    let pair = List.map (String.trim) pair in
    let pair = List.filter (fun x -> not (String.length x = 0)) pair in
      (List.hd pair, List.hd (List.tl pair))

let parse_pair_set (s : string) = 
  let pairs = String.trim s in
  let pairs = String.map (fun c -> if c = '{' || c = '}' then ';' else c) pairs in
  let pairs = String.split_on_char ';' pairs in
  let pairs = List.filter (fun x -> not (String.length x = 0)) pairs in
  let pairs = List.map (String.trim) pairs in
  List.map (parse_pair) pairs

(* Evaluate a command. *)
let rec evalc (conf:configuration) : (store * var_store * kripke_store) =
  match conf with
  | (sigma, var_sigma, k_st, Seq(c1, c2)) -> 
      let c1_eval_sigma, c1_eval_var_sigma, c1_eval_k_st = 
        evalc (sigma, var_sigma, k_st, c1) in
      evalc (c1_eval_sigma, c1_eval_var_sigma, c1_eval_k_st, c2)
  | (sigma, var_sigma, k_st, Assign(x, b)) -> 
      if List.mem x var_sigma
      then 
        if b <> Var x then (x, b)::sigma, var_sigma, k_st else raise (AssignVarToSelf x)
      else
        raise (UnboundVariable x)
  | (sigma, var_sigma, k_st, Print b) -> print_bexp b;
                        print_string(" = "); print_bexp (beval sigma var_sigma b);
                        print_string "\n"; 
                        sigma, var_sigma, k_st
  | (sigma, var_sigma, k_st, Intro x) -> sigma, x::var_sigma, k_st
  | (sigma, var_sigma, k_st, Intros x) -> 
    let variables = parse_set x in
      sigma, variables @ var_sigma, k_st
  | (sigma, var_sigma, k_st, CreateEmptyKripke x) ->
      sigma, var_sigma, (x, empty)::k_st
  | (sigma, var_sigma, k_st, AddWorldToKripke (x, w)) ->
    if List.exists (fun (v, _) -> v = x) k_st then
      sigma, var_sigma, 
      List.map 
      (fun (v, k) -> if v = x then (v, add_worlds k [w]) else (v, k)) k_st
    else 
      raise (UnboundedKripkeModel x)
  | (sigma, var_sigma, k_st, AddWorldsToKripke (x, w)) ->
    let variables = parse_set w in
      if List.exists (fun (v, _) -> v = x) k_st then
        sigma, var_sigma, 
        List.map 
        (fun (v, k) -> if v = x then (v, add_worlds k variables) else (v, k)) k_st
      else 
        raise (UnboundedKripkeModel x)
  | (sigma, var_sigma, k_st, AddAccessToKripke (x, (w1, w2))) ->
    if List.exists (fun (v, _) -> v = x) k_st then
      sigma, var_sigma,
      List.map 
      (fun (v, k) -> if v = x then (v, add_accessibility k (w1, w2)) else (v, k)) k_st
    else
      raise (UnboundedKripkeModel x)
  | (sigma, var_sigma, k_st, AddAccessesToKripke (x, world_pairs)) ->
    let pairs = parse_pair_set world_pairs in
    if List.exists (fun (v, _) -> v = x) k_st then
      sigma, var_sigma,
      List.map 
      (fun (v, k) -> if v = x then (v, add_accessibilities k pairs) else (v, k)) k_st
    else
      raise (UnboundedKripkeModel x)
  | (sigma, var_sigma, k_st, AddValuationToKripke (x, (p, w))) ->
    if List.exists (fun (v, _) -> v = x) k_st then
      sigma, var_sigma,
      List.map (fun (v, k) -> if v = x then (v, add_valuation k p w) else (v, k)) k_st
    else
      raise (UnboundedKripkeModel x)
  | (sigma, var_sigma, k_st, AddValuationsToKripke (x, (p, w))) ->
    let variables = parse_set p in
      if List.exists (fun (v, _) -> v = x) k_st then
        sigma, var_sigma,
        List.map (fun (v, k) -> if v = x then (v, add_valuations k variables w) else (v, k)) k_st
      else
        raise (UnboundedKripkeModel x)
  | (sigma, var_sigma, k_st, AssignMexp (x, GetTruthValueFromKripke (v, (w, m)))) ->
    if List.exists (fun (v', _) -> v' = v) k_st then 
      if List.mem x var_sigma then
        (x, eval_mexp (List.assoc v k_st) w m)::sigma, var_sigma, k_st
      else
        raise (UnboundVariable x)
    else
      raise (UnboundedKripkeModel x)
  | (sigma, var_sigma, k_st, LatexIt (x, f)) ->
    if List.exists (fun (v, _) -> v = x) k_st then begin
      latex_kripke (snd (List.find (fun (v, _) -> v = x) k_st)) f;
      (sigma, var_sigma, k_st) end
    else
      raise (UnboundedKripkeModel x)