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

(* Modal logic expressions *)
and mexp = 
| Bexp of bexp
| Square of mexp
| Diamond of mexp
