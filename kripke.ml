open Ast
open Set
open Eval

type world =  string

(* (w1, w2) in W <=> w2 is accessible from w1 *)
type access_relation = (world * world) set

(* if a world appears in the list associated to some propositional var
  we consider that variable to be true in that world
 *)
type valuation_function = (var * world set) set

type kripke = (world set * access_relation * valuation_function)

(* start off with empty model *)
let empty = (Set.empty, Set.empty, Set.empty)

let add_worlds (k : kripke) (w : world) =
  match k with
  | (w', r, v) -> let w_set = Set.singleton w in (union w_set w', r, v)

let add_accessibility (k : kripke) (world_pair : world * world) =
  match k with
  | (w, r, v) -> (w, Set.add_elt r world_pair, v)

let add_valuation (k : kripke) (x : var) (w : world) =
  let w_set = Set.singleton w in
  match k with
  | (worlds, r, v) -> 
    let current_valuation = Set.assoc_opt x v in
    (
      match current_valuation with
      | None -> (worlds, r, (x, w_set)::v)
      | Some curr_val -> (worlds, r, (x, union w_set curr_val)::(Set.remove_assoc x v))
    )

(* check if w2 is accessible from w1 *)
let rec check_accessibility (m : kripke) (w1 : world) (w2 : world) = 
  match m with
  | (worlds, r, v) -> let w = Set.assoc_opt w1 r in
    (
      match w with
      | None -> false
      | Some w'-> if w2 = w' then true else 
        let r' = Set.remove_assoc w1 r in 
        let m' = (worlds, r', v) in
        check_accessibility m' w1 w2
    )

(* return the set of worlds in m that are accessible from w *)
let rec accessible_worlds (m : kripke) (w : world) = 
  match m with
  | (w'::worlds, r, v) -> let m' = (worlds, r, v) in
    if check_accessibility m w w' then Set.add_elt (accessible_worlds m' w) w'
    else (accessible_worlds m' w)
  | ([], r, v) -> Set.empty

let rec world_to_store (m : kripke) (w : world) : Eval.store = 
  match m with
  | (worlds, r, v) -> List.map (fun (k, v') -> if List.mem w v' then (k, True) else (k, False)) v

let rec world_to_var_store (m : kripke) (w : world) : Eval.var_store = 
  let var_sigma, _ = List.split (world_to_store m w) in var_sigma

let rec check_valuation_at_world (m : kripke) (w : world) (e : mexp) : bexp = 
  match e with
  | Bexp e' -> 
    let sigma = world_to_store m w in
    let var_sigma = world_to_var_store m w in
    Eval.beval sigma var_sigma e'
  | Square e' -> 
    let a_worlds = accessible_worlds m w in
    true_at_all m a_worlds e'
  | Diamond e' -> 
    let a_worlds = accessible_worlds m w in
    true_at_some m a_worlds e'
and true_at_all (m : kripke) (worlds : world set) (e : mexp) =
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
and true_at_some (m : kripke) (worlds : world set) (e : mexp) = 
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

let eval_mexp (m : kripke) (w : world) (e : mexp) : bool = 
  match m with
  | (worlds, r, v) ->
    let sigma = world_to_store m w in
    let var_sigma = world_to_var_store m w in
    (* possibly sigma and var_sigma could both be [] because check_valuation_at_world 
    should only return a bexp containing {True, False, And, Or} *)
    let b = Eval.beval sigma var_sigma (check_valuation_at_world m w e) in
    match b with
    | True -> true
    | False -> false
    | _ -> failwith "e could not be evaluated to T/F"