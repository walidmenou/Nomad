val signatures : (Ast.ident * int * Ast.typ) list
(** Every built-in function: its name, how many arguments it takes and its type.
    The same type variable stands in each of them, since a scheme renames it at
    every use *)

val types : Infer.env
(** The typing environment a program starts in. [Eval.values] is the value
    environment that matches it *)
