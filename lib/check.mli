open Ast

val check_stmt : Infer.env -> statement -> Infer.env
(** Checks a statement and returns the environment the next one is checked in *)

val check_program : program -> unit
(** Checks every statement of a program in order, raising [Subst.TypeError] on
    the first one that does not check *)
