open Ast

val string_of_typ : typ -> string
(** Prints a type with each variable named after the counter that made it, as in
    ['a3]. Used in error messages, where two types share their variables and
    renaming either one on its own would mislead *)

val display_typ : typ -> string
(** Prints a type for a reader rather than for a diagnostic. The variables are
    renumbered from ['a] in order of appearance, so the name does not depend on
    how much inference ran before, and arrows group to the right without
    redundant parentheses, so a curried function reads [int -> int -> int] *)

val string_of_binop : binary_op -> string
(** Prints a binary operator the way the source writes it *)

val string_of_pat : pattern -> string
(** Prints a pattern the way the source writes it *)

val string_of_qual : qualifier -> string
(** Prints one comprehension qualifier the way the source writes it *)

val string_of_expr : expr -> string
(** Prints an expression as Nomad source, so an error can quote the program
    back. Anything that would not read as one unit in an operand or argument
    position is parenthesised, which keeps the output unambiguous without
    needing a table of operator precedences *)
