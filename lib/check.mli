open Ast

val convert : (ident * typ) list -> type_expr -> typ
(** The type a written type expression denotes, given a type for each variable
    it may mention. Rejects an unknown type name and a type applied to the wrong
    number of arguments *)

val declare : ident -> ident list -> (ident * type_expr list) list -> Infer.env
(** Records a type declaration and returns the scheme of each of its
    constructors. A constructor takes its arguments one at a time and returns
    the type being declared, so it is a curried function and may be applied to
    fewer arguments than it takes. The name is registered before the argument
    types are converted, which is what lets a declaration mention itself *)

val check_stmt : Infer.env -> statement -> Infer.env * typ
(** Checks a statement and returns the environment the next one is checked in,
    along with the type of the value it will produce. A type declaration
    produces nothing and has type [unit] *)

val check_program : program -> typ list
(** Checks every statement of a program in order and returns their types,
    raising [Subst.TypeError] on the first one that does not check. A type
    declaration contributes no type, since it produces no value to print *)
