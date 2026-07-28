open Ast

exception TypeError of string

(** Type environment mapping identifiers to their types. *)
type env = (ident * typ) list

(** Infers the type of an expression given an environment. *)
val infer : env -> expr -> typ

(** Type checks a statement and returns the updated environment. *)
val check_stmt : env -> statement -> env

(** Type checks an entire program. *)
val check_program : program -> unit

(** Helper to print types for error messages. *)
val string_of_typ : typ -> string
