# Nomad

A small, strict, statically typed functional language in the ML family, with borrow checking.

Nomad exists to state the answer to a programming challenge in as few lines as it takes to think of
it. No program carries a type annotation, because the checker works out every type on its own. One
rule about arrays gives constant-time updates without giving up value semantics.

## Build and run

Needs OCaml and dune, plus alcotest to run the tests.

```
make build          # builds ./nomad
./nomad file.nd     # run a program
./nomad             # start the prompt
make test           # 137 tests
```

## What is here

- Hindley-Milner type inference, with no annotation anywhere in the language
- List comprehensions with generators, guards and `let` qualifiers
- Algebraic data types with parameters, and a match that must cover every case
- Pattern matching over lists, tuples and constructors
- Arrays and grids under borrow checking, so an update is O(1) and still behaves as a value
- Chained comparison, so `0 <= x < 5` means what it means in mathematics
- A pipe operator, stepped ranges and layout-sensitive statements

## Sample Program

The 0/1 knapsack, whole. A comprehension whose body is an update fills an array instead of collecting
a list, and the descending range lets one row stand in for the table.

```
let knapsack cap items =
  let t = array (cap + 1) 0 in
  [t[w] := max t[w] (t[w - wt] + vl)
   | (wt, vl) <- items, w <- [cap, cap - 1..wt]][cap]
```

The update happens in place, and the borrow checker proves nothing can observe the array afterwards.

```
>> let a = array 3 0
{0; 0; 0}
>> let b = a[0] := 1
{1; 0; 0}
>> a[0]
Borrow Error: a was already given to an update
```
