open Kripke

(* Variables. *)
type var = string

type info = (int * int) * (int * int)

(* Boolean expressions. *)
and bexp =
| True
| False
| Implies of bexp * bexp
| Iff of bexp * bexp
| Not of bexp
| And of bexp * bexp
| Or of bexp * bexp
| Var of var
| Unknown of var * bexp

(* Commands. *)
and com =
| Assign of var * bexp
| Seq of com * com
| Print of bexp
| Intro of var

and kripke_bexp =
| GetTruthValueFromKripke of (world, bexp)

(* Commands specifically to interact with Kripke models. *)
and kripke_com =
| CreateEmptyKripke of var
| AddWorldToKripke of (var, world)
| AddAccessToKripke of (var, (world * world))
| AddValuationToKripke of (var, (var, world))
