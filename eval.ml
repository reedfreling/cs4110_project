open Ast
open Pprint

(* Interpreter exceptions. *)
exception UnboundVariable of var

(* A type for stores. *)
type store = (var * bexp) list

type var_store = (var) list

(* A type for configurations. *)
(* sigma, c, c', k *)
type configuration = (store * var_store * com)

let print_bexp b =
  print_string (strBexp b)

(* Create an initial configuration from a command. *)
let make_configuration (c:com) : configuration =
  ([], [], c)

(* Evaluate a boolean expression *)
let rec beval (sigma : store) (var_sigma : var_store) (b : bexp) : bexp =
  match b with
  | True -> True
  | False -> False
  | Not True -> False
  | Not False -> True
  | Not b -> Not (beval sigma var_sigma b)
  | And (True, b) -> beval sigma var_sigma b
  | And (False, _) -> False
  | And (b1, b2) -> beval sigma var_sigma (And (beval sigma var_sigma b1, b2))
  | Or (True, _) -> True
  | Or (False, b) -> beval sigma var_sigma b
  | Or (b1, b2) -> beval sigma var_sigma (Or (beval sigma var_sigma b1, b2))
  | Implies (b1, b2) -> beval sigma var_sigma (Or (Not b1, b2))
  | Iff (b1, b2) -> beval sigma var_sigma (And (Implies(b1, b2), Implies(b2, b1)))
  | Var x -> beval sigma var_sigma (
    match List.assoc_opt x sigma with
    | Some b -> b
    | None -> (
      if List.mem x var_sigma then failwith "unimplemented" else raise (UnboundVariable x)
    )
  )

(* Evaluate a command. *)
let rec evalc (conf:configuration) : (store * var_store) =
  match conf with
  | (sigma, var_sigma, Seq(c1, c2)) -> 
      let c1_eval_sigma, c1_eval_var_sigma = evalc (sigma, var_sigma, c1) in
      evalc (c1_eval_sigma, c1_eval_var_sigma, c2)
  | (sigma, var_sigma, Assign(x, b)) -> 
      (x, beval sigma var_sigma b)::sigma, var_sigma
  | (sigma, var_sigma, Print b) -> print_bexp (beval sigma var_sigma b); 
                        print_string "\n"; 
                        sigma, var_sigma
  | (sigma, var_sigma, Intro x) -> sigma, x::var_sigma