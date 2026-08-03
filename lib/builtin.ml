open Ast

let v = Infer.fresh_var ()
let a = TVar v
let arrow ts r = List.fold_right (fun t acc -> TArrow (t, acc)) ts r

let signatures =
  [
    ("array", 2, arrow [ TInt; a ] (TArray a));
    ("size", 1, arrow [ TArray a ] TInt);
    ("copy", 1, arrow [ TArray a ] (TArray a));
    ("from_list", 1, arrow [ TList a ] (TArray a));
  ]

let types =
  List.map (fun (name, _, t) -> (name, Infer.Forall ([ v ], t))) signatures
