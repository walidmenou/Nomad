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
    what it does. And neither a function written inline nor a comprehension may
    write to an array, since both may run more than once *)
