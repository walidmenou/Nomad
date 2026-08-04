open Ast

exception BorrowError of string

val check_program : program -> unit
(** Checks that no array is read after it has been given away, which is what
    makes an update safe to perform in place.

    Reading with [a[i]] borrows the array and leaves it usable. Writing with
    [a[i] := v] gives it away, and so does passing it to a function that writes
    to it. A name that has been given away may not be used again, and [copy] is
    how a program keeps a version it still needs.

    Which arguments a function gives away is worked out from its body rather
    than written down, so types are unchanged. Two restrictions keep that sound.
    A function that writes to an argument has to be called with all of its
    arguments rather than passed around as a value, since nothing then knows
    what it does. And a function written inline may not write to an array, since
    it may run more than once.

    A comprehension whose body is an update is the exception, because it is what
    produces the array. It may write to exactly one array, which has to be bound
    outside it, and it gives that array away *)
