open Ast

val exhaustive : typ -> pattern list -> bool
(** Whether the patterns between them match every value of the type. Decided by
    Maranget's usefulness algorithm: the patterns are exhaustive exactly when a
    further wildcard row would be useless, that is when no value escapes them
    all.

    The type decides which sets of constructors are complete. Both booleans make
    a set complete, as do [[]] and [::] together, while integers and strings are
    too many to ever cover without a wildcard, and a type with no pattern form
    of its own can only be matched by one.

    A tuple has just one constructor, so a single tuple pattern of the right
    width covers the type and the question moves to its parts *)
