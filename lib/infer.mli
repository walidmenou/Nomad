open Ast

type env = (ident * typ) list
(** The type each identifier in scope is known to have *)

val fresh : unit -> typ
(** A type variable that no other expression has been given *)

val infer_w : env -> expr -> Subst.t * typ
(** Algorithm W: the substitution an expression forces on its environment,
    together with its type *)

val infer : env -> expr -> typ
(** The type of an expression, raising [Subst.TypeError] if it has none *)
