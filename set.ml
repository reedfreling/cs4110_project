type 'a set = 'a list

let empty = []

let add_elt lst elt =
  if List.mem elt lst then lst else elt::lst

let singleton w = 
  add_elt empty w

let union lst1 lst2 =
  List.fold_left (fun acc elt -> if List.mem elt acc then acc else elt::acc) lst1 lst2

let intersection lst1 lst2 =
  List.filter (fun elt -> List.mem elt lst2) lst1

let assoc = List.assoc

let assoc_opt = List.assoc_opt

let remove_assoc = List.remove_assoc