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
    let variables = String.trim w in
    let variables = String.map (fun c -> if c = '{' || c = '}' then ',' else c) variables in
    let variables = String.split_on_char ',' variables in
    let variables = List.map (String.trim) variables in
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
  | (sigma, var_sigma, k_st, AddValuationToKripke (x, (p, w))) ->
    if List.exists (fun (v, _) -> v = x) k_st then
      sigma, var_sigma,
      List.map (fun (v, k) -> if v = x then (v, add_valuation k p w) else (v, k)) k_st
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