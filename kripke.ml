open Ast
open Set

type world =  string

type access_relation = (world * world) set

(* if a world appears in the list associated to some propositional var
  we consider that variable to be true in that world
 *)
type valuation_function = (var * world set) set

type kripke = (world set * access_relation * valuation_function)

(* start off with empty model *)
let empty = (Set.empty, Set.empty, Set.empty)

let add_worlds (k : kripke) (w : world set) =
  match k with
  | (w', r, v) -> (union w w', r, v)

let add_accessibility (k : kripke) (world_pair : world * world) =
  match k with
  | (w, r, v) -> (w, Set.add_elt r world_pair, v)

let add_valuation (k : kripke) (x : var) (w : world set) =
  match k with
  | (worlds, r, v) -> 
    let current_valuation = Set.assoc_opt x v in
    (
      match current_valuation with
      | None -> (worlds, r, (x, w)::v)
      | Some curr_val -> (worlds, r, (x, union w curr_val)::(Set.remove_assoc x v))
    )