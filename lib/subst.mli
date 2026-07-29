open Ast

exception TypeError of string

type t = (int * typ) list
(** What each type variable has been found to stand for *)

val apply : t -> typ -> typ
(** Replaces every type variable in the type by what it stands for *)

val compose : t -> t -> t
(** [compose s1 s2] is [s2] followed by [s1], with [s1] taking precedence *)

val unify : typ -> typ -> t
(** The most general substitution that makes the two types equal, raising
    [TypeError] when no such substitution exists *)
