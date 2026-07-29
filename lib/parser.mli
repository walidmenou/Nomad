open Ast

type 'a t = char list -> ('a * char list) option
(** Monad Type *)

val run : 'a t -> string -> ('a, string) result
(** Monad Run Function, applies the parser `p` to the string `s` *)

val return : 'a -> 'a t
(** Wrapper Function: always succeeds without consuming the input *)

val none : 'a t
(** Wrapper Function: always returns None *)

val ( |*> ) : 'a t -> ('a -> 'b t) -> 'b t
(** Sequence Operator: sequences two parsers *)

val ( <|> ) : 'a t -> 'a t -> 'a t
(** Choice Operator: Runs the first parser, fallsback on the second *)

val satisfies : (char -> bool) -> char t
(** Parses a single character matching a predicate *)

val char : char -> char t
(** Parses a specific character *)

val digit : int t
(** Parses a single digit and returns its integer value *)

val ( |>> ) : 'a t -> 'b t -> 'b t
(** Sequences two parsers ignoring the result of the left/right parser *)

val ( <<| ) : 'a t -> 'b t -> 'a t

val many : 'a t -> 'a list t
(** Repeats the given parser zero or more times *)

val some : 'a t -> 'a list t
(** Repeats the given parser one or more times *)

val sepby : 'a t -> 'b t -> 'b list t
(** Parses multiple instances of the first parser separated by instances of the
    second parser *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Applies the given function to the result of the parser *)

val maybe : 'a t -> 'a option t
(** Always succeeds, returns `None` if the given parser fails and Some x if it
    succeds *)

val nat : int t
(** Parses a string representing a number and converts it to an integer *)

val natural : int t
(** Parses a natural number and consumes all spaces that come after it *)

val integer : int t
(** Parses an integer (positive or negative) and consumes all spaces that come
    after it *)

val spaces : char list t
(** Parses a sequence of contiguous spaces *)

val token : 'a t -> 'a t
(** Runs the parser and consumes all spaces that come after it *)

val keyword : string -> unit t
(** Parses the given keyword string *)

val between : 'a t -> 'b t -> 'c t -> 'c t
(** Parses the expression between the first two parsers *)

val parenthesized : 'a t -> 'a t
(** Parses expressions of the form: `( e )` *)

val alpha : char t
(** Parses a lowercase or uppercase character *)

val alphanumeric : char t
(** Parses a lowercase or uppercase character or a digit *)

val ident : string t
(** parses an identifier: alpha character followed by alphanumeric characters *)

val int_lit : expr t
(** Parses an integer Literal expression, e.g: "123" *)

val bool_lit : expr t
(** Parses an boolean Literal expression, e.g: "true"*)

val unit_lit : expr t
(** Parses () *)

val addop : binary_op t
(** Parses an addition or subtraction operator *)

val mulop : binary_op t
(** Parses a multiplication or division operator *)

val chain_left : binary_op t -> expr t -> expr t
(** Parses a chain of expressions separated by the given operator type *)

val or_expr : expr t
(** Parses a chain of disjunctions, the loosest operator level *)

val and_expr : expr t
(** Parses a chain of conjunctions *)

val cons_expr : expr t
(** Parses a chain of conses, e.g: `1 :: 2 :: []` *)

val add_expr : expr t
(** Parses an additive expression *)

val mul_expr : expr t
(** Parses a multiplicative expression *)

val var_expr : expr t
(** Parses a reference to a variable *)

val lit_expr : expr t
(** Parses a literal, e.g: 12, true or () *)

val list_expr : expr t
(** Parses a list expression *)

val atom_expr : expr t
(** Parses an atomic (i.e irreducile) expression, e.g: `x`, `(<expr>)`, `12` *)

val cmp_expr : expr t
(** Parses a chain of comaprisons, e.g: 0 <= x < 12 < 2 *)

val let_expr : expr t
(** Parses a `let ... = ... in ...` expression *)

val if_expr : expr t
(** Parses an `if ... then ... else ...` expression *)

val fun_expr : expr t
(** Parses a lambda function expression *)

val app_expr : expr t
(** Parses the application of a function f on an expression e *)

val expr : expr t
(** Parses an expression *)

val pipe : unit t
(** Parse a pipe operator `|>`*)

val pipe_expr : expr t
(** Parses chained pipe operations, e.g: `f1 |> f2 |> f3 |> ... |> fn` *)

val match_expr : expr t
(** Parses a pattern matching expression *)

val wildcard_pat : pattern t
(** Parses a wildcard pattern `_` *)

val var_pat : pattern t
(** Parses a variable pattern *)

val int_pat : pattern t
(** Parses an integer pattern *)

val bool_pat : pattern t
(** Parses a boolean pattern *)

val string_pat : pattern t
(** Parses a string pattern *)

val pattern : pattern t
(** Parses any pattern *)

val let_rec_expr : expr t
(** Parses a `let rec ... = ... in ...` expression *)

val statement : statement t
(** Parses a single statement *)

val program : program t
(** Parses a program consisting of a list of statements *)
