open Ast

exception EvaluationError of string

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
  | VArray of value array
  | VTuple of value list
  | VClos of ident * expr * env
  | VRecClos of ident * ident * expr * env
  | VCon of ident * int * value list
  | VBuiltin of ident * int * value list
      (** What an expression evaluates to. A closure carries the environment its
          function was written in, so a free variable in the body means what it
          meant there rather than what it means at the call. A recursive closure
          also carries the name the function calls itself by, which is how
          recursion works without mutation. A constructor carries its name, the
          number of arguments it takes and the ones it has been given, so it
          behaves as a function until it has them all, and a built-in collects
          its arguments the same way before it runs *)

and env = (ident * value) list
(** The value each identifier in scope stands for *)

val values : env
(** The value environment a program starts in, holding one value for each of
    [Builtin.signatures] *)

val eval : env -> expr -> value
(** The value of an expression, raising [EvaluationError] if it has none.
    Operands are evaluated left to right, except that [&&] and [||] look at the
    right one only when the left has not already settled the answer.

    A comprehension runs its qualifiers left to right and nests them, so the
    leftmost generator is the outer loop and a later qualifier sees every name
    an earlier one bound. A generator pattern that a value does not match skips
    that value rather than failing. The language is strict, so the whole result
    is built before anything else happens *)

val eval_stmt : env -> statement -> env * value
(** Evaluates a statement and returns the environment the next one runs in,
    along with the value it produced. A type declaration binds each of its
    constructors and produces nothing *)

val show_val : typ -> value -> string
(** Prints a value the way the source would write it. A function has no useful
    form of its own, so it prints as [<fun>] followed by its type, which is the
    informative part, and a constructor that is still short of arguments prints
    the same way. The type comes from the checker, since the evaluator does not
    compute one *)

val run_program : program -> value list
(** The value each statement of a program produces, in order, without printing
    anything. A type declaration contributes nothing *)

val eval_program : typ list -> program -> unit
(** Runs every statement of a program in order and prints each value against the
    type the checker gave it *)
