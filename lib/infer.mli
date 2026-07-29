open Ast

type env = (ident * typ) list
(** The type each identifier in scope is known to have *)

val rec_binding : env -> ident -> expr -> Subst.t * typ
(** Infers a recursive binding [id = e]: the substitution it forces, and the
    type [id] ends up with *)

val infer : env -> expr -> typ
(** The type of an expression, raising [Subst.TypeError] if it has none *)
