open Ast

exception BorrowError of string

type state
(** What the checker carries from one statement to the next: which arrays have
    been given away, and which arguments each function gives away *)

val initial : state
(** The state a program starts in *)

val check_stmt : state -> statement -> state
(** Checks one statement and returns the state the next one is checked in, which
    is what lets the prompt check a line at a time *)

val check_program : program -> unit
(** Checks that no array is read after it has been given away, which is what
    makes an update safe to perform in place.

    Reading with [a[i]] borrows the array and leaves it usable. Writing with
    [a[i] := v] gives it away, and so does passing it to a function that writes
    to it. A name that has been given away may not be used again, and [copy] is
    how a program keeps a version it still needs.

    Binding one name to another, as [let b = a] does, makes both names stand for
    the same array, so giving away either gives away both. The same holds when
    an array reaches a name through a branch, through a function that returns
    what it was handed, or through a function that returns an array it closed
    over. A built-in never returns an array it was given, so [copy] starts a
    name of its own.

    Which arguments a function gives away is worked out from its body rather
    than written down, so types are unchanged. Two restrictions keep that sound.
    A function that writes to an argument has to be called with all of its
    arguments rather than passed around as a value, since nothing then knows
    what it does. And a function written inline may not write to an array, since
    it may run more than once.

    A comprehension whose body is an update is the exception, because it is what
    produces the array. It may write to exactly one array, which has to be bound
    outside it, and it gives that array away *)
