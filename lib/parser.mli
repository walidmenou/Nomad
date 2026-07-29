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
    after it. Used by patterns, where a sign cannot be an operator *)

val spaces : unit t
(** Parses whitespace between statements, newlines included, since a newline
    carries no meaning there. A comment runs from `--` to the end of the line
    and counts as whitespace too *)

val inline_spaces : unit t
(** Parses the whitespace that belongs to one statement. A newline is part of it
    only when the line that follows is indented, so a token in the first column
    always begins a new statement. Blank lines and comment lines are looked past
    when deciding that, so a comment can sit in the first column without ending
    the statement above it *)

val token : 'a t -> 'a t
(** Runs the parser and consumes the whitespace within a statement that comes
    after it *)

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
(** Parses an integer literal expression, e.g: "123". A literal carries no sign,
    since a leading minus is the unary operator, so that `1 -2` reads as a
    subtraction rather than an application *)

val bool_lit : expr t
(** Parses an boolean Literal expression, e.g: "true"*)

val unit_lit : expr t
(** Parses () *)

val addop : binary_op t
(** Parses an addition or subtraction operator *)

val mulop : binary_op t
(** Parses a multiplication or division operator *)

val orop : binary_op t
(** Parses a disjunction operator *)

val andop : binary_op t
(** Parses a conjunction operator *)

val cmpop : binary_op t
(** Parses a relational or equality operator. Two-character operators are tried
    first, so that `<=` is never read as `<` followed by `=` *)

val consop : binary_op t
(** Parses a cons operator *)

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

val neg_expr : expr t
(** Parses a negation, e.g: `-x`. Binds tighter than multiplication and stands
    for `0 - x`, so nothing downstream needs to know about it *)

val var_expr : expr t
(** Parses a reference to a variable *)

val lit_expr : expr t
(** Parses a literal, e.g: 12, true or () *)

val list_expr : expr t
(** Parses a bracketed expression, either a list literal `[e; ...; e]` or an
    inclusive integer range `[a..b]`. The range is empty when its upper bound is
    below its lower one *)

val atom_expr : expr t
(** Parses an atomic (i.e irreducile) expression, e.g: `x`, `(<expr>)`, `12` *)

val cmp_expr : expr t
(** Parses a chain of comaprisons, e.g: 0 <= x < 12 < 2 *)

val let_expr : expr t
(** Parses a `let f x y = ... in ...` expression. Parameters after the name are
    sugar for nested functions, so the binding is always to a single value *)

val if_expr : expr t
(** Parses an `if ... then ... else ...` expression *)

val fun_expr : expr t
(** Parses a lambda expression, e.g: `fun x y -> ...`. Several parameters are
    sugar for a function that returns a function *)

val app_expr : expr t
(** Parses the application of a function f on an expression e *)

val expr : expr t
(** Parses an expression. Each operator level is one parser written in terms of
    the level that binds tighter. Loosest to tightest: `||`, `&&`, comparison,
    `|>`, `::`, `+ -`, `* /`, application *)

val pipe : unit t
(** Parse a pipe operator `|>`*)

val pipe_expr : expr t
(** Parses chained pipe operations, e.g: `f1 |> f2 |> f3 |> ... |> fn`. Sits
    above cons and below comparison, so that a whole pipeline is one operand of
    the comparison around it *)

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
(** Parses a `let rec f x y = ... in ...` expression *)

val statement : statement t
(** Parses a single statement *)

val program : program t
(** Parses a program consisting of a list of statements. One statement ends
    where the next line begins in the first column, or at an optional `;;` *)
