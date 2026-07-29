open Ast

exception EvaluationError of string

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
  | VClos of ident * expr * env
  | VRecClos of ident * ident * expr * env
      (** What an expression evaluates to. A closure carries the environment its
          function was written in, so a free variable in the body means what it
          meant there rather than what it means at the call. A recursive closure
          also carries the name the function calls itself by, which is how
          recursion works without mutation *)

and env = (ident * value) list
(** The value each identifier in scope stands for *)

val eval : env -> expr -> value
(** The value of an expression, raising [EvaluationError] if it has none.
    Operands are evaluated left to right, except that [&&] and [||] look at the
    right one only when the left has not already settled the answer *)

val eval_stmt : env -> statement -> env * value
(** Evaluates a statement and returns the environment the next one runs in,
    along with the value it produced *)

val show_val : value -> string
(** Prints a value the way the source would write it. A function prints as
    [<fun>], since there is nothing useful to show *)

val run_program : program -> string list
(** The lines a program prints, in order, without printing them *)

val eval_program : program -> unit
(** Runs every statement of a program in order and prints each value *)
