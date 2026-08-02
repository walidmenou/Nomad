open Ast

type decl = { params : int list; cons : (ident * typ list) list }
(** A declared type: the variables it takes, written as the type variables the
    checker allocated for them, and each constructor with the types of the
    arguments it takes *)

val declare : ident -> decl -> unit
(** Records a declaration under its name, replacing any earlier one. The name is
    registered before its constructors are converted, so a constructor may
    mention the type being declared *)

val find : ident -> decl option
(** The declaration of a type name, if there is one *)

val reset : unit -> unit
(** Forgets every declaration, which is what a fresh run needs *)
