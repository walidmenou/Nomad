open Ast

type scheme =
  | Forall of int list * typ
      (** A type together with the variables in it that stand for any type at
          all. Each use of the name gets a fresh copy of those, which is what
          lets one binding serve several types *)

type env = (ident * scheme) list
(** The scheme each identifier in scope was given *)

val fresh_var : unit -> int
(** The name of a type variable no other part of the program is using. A scheme
    has to quantify one of these rather than a number chosen by hand, since
    instantiating a variable that maps to itself would not terminate *)

val fresh : unit -> typ
(** A type variable no other part of the program is using *)

val mono : typ -> scheme
(** The scheme of a type that stands for itself and nothing else, which is what
    a function parameter and a pattern variable get *)

val generalize : env -> typ -> scheme
(** Quantifies the variables of the type that the environment does not already
    constrain. Applied to a [let] binding and to a top level one *)

val instantiate : scheme -> typ
(** A fresh copy of the type, one new variable for each quantified one *)

val apply_env : Subst.t -> env -> env
(** Applies the substitution to every scheme in an environment, leaving what
    each one quantifies alone *)

val infer_w : env -> expr -> Subst.t * typ
(** Infers the type of an expression, returning the substitution it forces
    together with the type it found *)

val rec_binding : env -> ident -> expr -> Subst.t * typ
(** Infers a recursive binding [id = e]: the substitution it forces, and the
    type [id] ends up with *)

val infer : env -> expr -> typ
(** The type of an expression, raising [Subst.TypeError] if it has none *)
