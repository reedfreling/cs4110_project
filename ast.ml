(* Variables. *)
type var = string

type world = string

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
| AssignMexp of var * kripke_bexp
| Seq of com * com
| Print of bexp
| Intro of var
| CreateEmptyKripke of var
| AddWorldToKripke of (var * world)
| AddAccessToKripke of (var * (world * world))
| AddValuationToKripke of (var * (var * world))

(* Modal logic expressions *)
and mexp = 
| Bexp of bexp
| Square of mexp
| Diamond of mexp

and kripke_bexp =
| GetTruthValueFromKripke of (var * (world * mexp))
