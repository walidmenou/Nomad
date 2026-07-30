open Ast

val check_stmt : Infer.env -> statement -> Infer.env * typ
(** Checks a statement and returns the environment the next one is checked in,
    along with the type of the value it will produce *)

val check_program : program -> typ list
(** Checks every statement of a program in order and returns their types,
    raising [Subst.TypeError] on the first one that does not check *)
