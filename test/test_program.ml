open Nomad

let run src =
  match Parser.run Parser.program src with
  | Error e -> Alcotest.fail ("Parse error: " ^ e)
  | Ok stmts ->
      let types = Check.check_program stmts in
      List.map2 Eval.show_val types (Eval.run_program stmts)

let check_run src expected () =
  Alcotest.(check (list string)) "output" expected (run src)

let check_eval_error src expected () =
  match run src with
  | _ -> Alcotest.fail "expected an evaluation error"
  | exception Eval.EvaluationError e ->
      Alcotest.(check string) "error message" expected e

let check_type_error src expected () =
  match run src with
  | _ -> Alcotest.fail "expected a type error"
  | exception Subst.TypeError e ->
      Alcotest.(check string) "error message" expected e

let check_type_fails src () =
  match run src with
  | _ -> Alcotest.fail "expected a type error"
  | exception Subst.TypeError _ ->
      Alcotest.(check bool) "failed as expected" true true

let read_file path =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let test_bindings () =
  check_run "let x = 1" [ "1" ] ();
  check_run "let x = 1\nlet y = x + 1" [ "1"; "2" ] ();
  check_run "let x = 1\nlet x = 2\nlet y = x" [ "1"; "2"; "2" ] ()

let test_layout () =
  check_run "let id = fun x -> x\nid 1\nid true"
    [ "<fun> : 'a -> 'a"; "1"; "true" ]
    ();
  check_run "1 + 1\n2 + 2" [ "2"; "4" ] ();
  check_run "let a =\n  1 +\n  2" [ "3" ] ();
  check_run "let a =\n  let x = 1 in\n  x + 1" [ "2" ] ();
  check_run "let a = 1\n\nlet b = 2" [ "1"; "2" ] ();
  check_run "let a =\n  1 +\n\n  2" [ "3" ] ();
  check_run "\n\nlet a = 1\n\n" [ "1" ] ()

let test_double_semicolon () =
  check_run "let a = 1 ;; let b = 2" [ "1"; "2" ] ();
  check_run "let a = 1 ;; a + 1 ;; a + 2" [ "1"; "2"; "3" ] ();
  check_run "let a = 1 ;;" [ "1" ] ();
  check_run "let a = 1\nlet b = 2 ;;" [ "1"; "2" ] ()

let test_comments () =
  check_run "let a = 1 -- trailing" [ "1" ] ();
  check_run "-- a header\nlet a = 1" [ "1" ] ();
  check_run "let a = 1\n-- between statements\nlet b = 2" [ "1"; "2" ] ();
  check_run "let a =\n  1 + -- add\n  2" [ "3" ] ();
  check_run "let a =\n  1 +\n-- in the first column\n  2" [ "3" ] ();
  check_run "-- nothing at all" [] ();
  check_run "let s = \"a--b\"" [ "\"a--b\"" ] ()

let test_range () =
  check_run "let a = [1..5]" [ "[1; 2; 3; 4; 5]" ] ();
  check_run "let a = [3..3]" [ "[3]" ] ();
  check_run "let a = [5..1]" [ "[]" ] ();
  check_run "let a = [-2..2]" [ "[-2; -1; 0; 1; 2]" ] ();
  check_run "let n = 4\nlet a = [1..n - 1]" [ "4"; "[1; 2; 3]" ] ();
  check_run "let a = 0 :: [1..3]" [ "[0; 1; 2; 3]" ] ();
  check_type_error "let a = [1..true]" "Type mismatch: bool and int" ()

let test_comprehension () =
  check_run "let a = [x | x <- [1; 2; 3]]" [ "[1; 2; 3]" ] ();
  check_run "let a = [x * x | x <- [1..4]]" [ "[1; 4; 9; 16]" ] ();
  check_run "let xs = [1; 2]\nlet a = [x + 1 | x <- xs]" [ "[1; 2]"; "[2; 3]" ]
    ();
  check_run "let a = [\"s\" | x <- [1; 2]]" [ "[\"s\"; \"s\"]" ] ();
  check_run "let a = [x | x <- []]" [ "[]" ] ()

let test_nested_generators () =
  check_run "let a = [x * 10 + y | x <- [1; 2], y <- [3; 4]]"
    [ "[13; 14; 23; 24]" ] ();
  check_run "let a = [[x; y] | x <- [1; 2], y <- [3]]" [ "[[1; 3]; [2; 3]]" ] ();
  check_run "let a = [x + y | x <- [1..3], y <- [1..x]]"
    [ "[2; 3; 4; 4; 5; 6]" ] ();
  check_run "let a = [x | x <- [1; 2], y <- []]" [ "[]" ] ()

let test_generator_patterns () =
  check_run "let a = [x | x :: r <- [[1; 2]; [3]]]" [ "[1; 3]" ] ();
  check_run "let a = [x | x :: r <- [[1]; []; [3]]]" [ "[1; 3]" ] ();
  check_run "let a = [1 | [] <- [[1]; []; []]]" [ "[1; 1]" ] ()

let test_comprehension_guards () =
  check_run "let a = [x | x <- [1..6], x > 3]" [ "[4; 5; 6]" ] ();
  check_run "let a = [x | x <- [1..10], x > 3, x < 6]" [ "[4; 5]" ] ();
  check_run "let a = [x * x | x <- [1..5], x < 3]" [ "[1; 4]" ] ();
  check_run "let a = [x | x <- [1..3], false]" [ "[]" ] ();
  check_run "let a = [x + y | x <- [1..3], x > 1, y <- [1..2]]"
    [ "[3; 4; 4; 5]" ] ();
  check_run "let a = [x | x <- [1..4], x <> 2]" [ "[1; 3; 4]" ] ();
  check_type_fails "let a = [x | x <- [1..3], x]" ()

let test_comprehension_let () =
  check_run "let a = [y | x <- [1..3], let y = x * x]" [ "[1; 4; 9]" ] ();
  check_run "let a = [y | x <- [1..5], let y = x * x, y > 8]" [ "[9; 16; 25]" ]
    ();
  check_run "let a = [y + z | x <- [1..2], let y = x + 1, let z = y * 2]"
    [ "[6; 9]" ] ();
  check_run "let a = [y | x <- [1..2], let y = x, y <- [1..y]]" [ "[1; 1; 2]" ]
    ();
  check_run "let x = 100\nlet a = [x | y <- [1], let x = 1]" [ "100"; "[1]" ] ()

let test_comprehension_programs () =
  check_run
    "let a = [[x; y; z] | z <- [1..13], y <- [1..z], x <- [1..y], x * x + y * \
     y = z * z]"
    [ "[[3; 4; 5]; [6; 8; 10]; [5; 12; 13]]" ]
    ();
  check_run "let a = [x | r <- [[1; 2]; [3]; []], x <- r]" [ "[1; 2; 3]" ] ();
  check_run
    "let rec any p l = match l with [] -> false | x :: r -> p x || any p r\n\
     let a = [n | n <- [2..12], any (fun d -> n / d * d = n) [2..n - 1] = \
     false]"
    [ "<fun> : ('a -> bool) -> 'a list -> bool"; "[2; 3; 5; 7; 11]" ]
    ()

let test_comprehension_body () =
  check_run "let a = [match x with 0 -> 9 | n -> n | x <- [0; 1; 2]]"
    [ "[9; 1; 2]" ] ();
  check_run "let a = [if x < 2 then 0 else x | x <- [1..3]]" [ "[0; 2; 3]" ] ();
  check_run "let a = [x > 1 && x < 3 | x <- [1..3]]" [ "[false; true; false]" ]
    ();
  check_run "let n = 3\nlet a = [[0 | j <- [1..n]] | i <- [1..n]]"
    [ "3"; "[[0; 0; 0]; [0; 0; 0]; [0; 0; 0]]" ]
    ()

let test_comprehension_types () =
  check_type_fails "let a = [x | x <- 5]" ();
  check_type_error "let a = [x + 1 | x <- [true]]" "Type mismatch: bool and int"
    ()

let test_tuple () =
  check_run "let a = (1, 2)" [ "(1, 2)" ] ();
  check_run "let a = (1, true, \"s\")" [ "(1, true, \"s\")" ] ();
  check_run "let a = (1, 2, 3, 4, 5)" [ "(1, 2, 3, 4, 5)" ] ();
  check_run "let a = ((1, 2), 3)" [ "((1, 2), 3)" ] ();
  check_run "let a = (1, [2; 3])" [ "(1, [2; 3])" ] ();
  check_run "let a = [(1, 2); (3, 4)]" [ "[(1, 2); (3, 4)]" ] ();
  check_run "let a = (1)" [ "1" ] ();
  check_run "let a = ()" [ "()" ] ()

let test_tuple_equality () =
  check_run "let a = (1, 2) = (1, 2)" [ "true" ] ();
  check_run "let a = (1, 2) = (1, 3)" [ "false" ] ();
  check_run "let a = (1, \"s\") <> (1, \"t\")" [ "true" ] ()

let test_tuple_types () =
  check_run "let pair x = (x, x)\nlet a = pair 1\nlet b = pair \"s\""
    [ "<fun> : 'a -> ('a * 'a)"; "(1, 1)"; "(\"s\", \"s\")" ]
    ();
  check_run "let a = [(x, y) | x <- [1; 2], y <- [3]]" [ "[(1, 3); (2, 3)]" ] ();
  check_type_fails "let a = (1, 2) = (1, true)" ();
  check_type_fails "let a = (1, 2) = (1, 2, 3)" ()

let test_tuple_patterns () =
  check_run "let fst p = match p with (a, b) -> a\nlet a = fst (1, 2)"
    [ "<fun> : ('a * 'b) -> 'a"; "1" ]
    ();
  check_run "let snd p = match p with (a, b) -> b\nlet a = snd (1, \"s\")"
    [ "<fun> : ('a * 'b) -> 'b"; "\"s\"" ]
    ();
  check_run "let a = match (1, 2, 3) with (x, y, z) -> x + y + z" [ "6" ] ();
  check_run "let a = match ((1, 2), 3) with ((x, y), z) -> x + y + z" [ "6" ] ();
  check_run "let a = match (1, 2) with (1, y) -> y | _ -> 0" [ "2" ] ();
  check_run "let a = match (5, 2) with (1, y) -> y | (x, y) -> x * y" [ "10" ]
    ();
  check_run "let a = [a + b | (a, b) <- [(1, 2); (3, 4)]]" [ "[3; 7]" ] ()

let test_tuple_exhaustiveness () =
  check_run "let a = match (1, 2) with (x, y) -> x" [ "1" ] ();
  check_run "let a = match (true, 1) with (true, y) -> y | (false, y) -> 0"
    [ "1" ] ();
  check_type_error "let a = match (true, 1) with (true, y) -> y"
    "This match is not exhaustive" ();
  check_type_error "let a = match (1, 2) with (1, y) -> y"
    "This match is not exhaustive" ();
  check_type_fails "let a = match (1, 2) with (x, y, z) -> x" ();
  check_type_error "let a = match (1, 2) with (x, x) -> x"
    "x is bound twice in the same pattern" ()

let test_parenthesised_patterns () =
  check_run "let a = match [1; 2] with (x :: xs) -> x | [] -> 0" [ "1" ] ();
  check_run "let a = match [[1; 2]] with (x :: xs) :: r -> x | _ -> 0" [ "1" ]
    ()

let test_declarations () =
  check_run "type shape = Circle int | Rect int int\nlet a = Circle 5"
    [ "Circle 5" ] ();
  check_run "type shape = Circle int | Rect int int\nlet a = Rect 3 4"
    [ "Rect 3 4" ] ();
  check_run "type colour = Red | Green\nlet a = Red\nlet b = [Red; Green]"
    [ "Red"; "[Red; Green]" ] ()

let test_declaration_types () =
  check_run "type shape = Circle int\nlet a = Circle" [ "<fun> : int -> shape" ]
    ();
  check_run "type 'a box = Box 'a\nlet a = Box 1\nlet b = Box \"s\""
    [ "Box 1"; "Box \"s\"" ] ();
  check_run "type 'a box = Box 'a\nlet wrap x = Box x"
    [ "<fun> : 'a -> 'a box" ] ();
  check_run "type ('a, 'b) either = Left 'a | Right 'b\nlet a = Left 1"
    [ "Left 1" ] ();
  check_run "type 'a box = Box 'a\nlet a = Box [[1]]\nlet b = Box (Box 1)"
    [ "Box [[1]]"; "Box (Box 1)" ]
    ();
  check_run
    "type 'a option = None | Some 'a\n\
     type t = S (int option list)\n\
     let a = S [Some 1; None]"
    [ "S [Some 1; None]" ] ()

let test_recursive_declarations () =
  check_run "type tree = Leaf | Node tree int tree\nlet t = Node Leaf 1 Leaf"
    [ "Node Leaf 1 Leaf" ] ();
  check_run
    "type tree = Leaf | Node tree int tree\n\
     let t = Node (Node Leaf 1 Leaf) 2 Leaf"
    [ "Node (Node Leaf 1 Leaf) 2 Leaf" ]
    ()

let test_declaration_equality () =
  check_run
    "type shape = Circle int | Rect int int\nlet a = Circle 5 = Circle 5"
    [ "true" ] ();
  check_run
    "type shape = Circle int | Rect int int\nlet a = Circle 5 = Rect 3 4"
    [ "false" ] ()

let test_declaration_errors () =
  check_type_error "type shape = Circle int\nlet a = Circle true"
    "In Circle true: Type mismatch: int and bool" ();
  check_type_error "let a = Nope 1" "Unbound variable Nope" ();
  check_type_error "type t = A bogus" "Unknown type bogus" ();
  check_type_error "type t = A 'z" "Unbound type variable 'z" ();
  check_type_error
    "type ('a, 'b) either = Left 'a | Right 'b\ntype t = A (int either)"
    "Wrong number of arguments for either" ();
  check_type_error "type t = A (int list)\nlet a = A 1"
    "In A 1: Type mismatch: int list and int" ();
  check_type_fails "type 'a box = Box 'a\nlet a = [Box 1; Box \"s\"]" ()

let test_printing () =
  check_run "let s = \"hi\"" [ "\"hi\"" ] ();
  check_run "let u = ()" [ "()" ] ();
  check_run "let l = [1; 2; 3]" [ "[1; 2; 3]" ] ();
  check_run "let l = []" [ "[]" ] ();
  check_run "let l = [[1]; [2; 3]]" [ "[[1]; [2; 3]]" ] ()

let test_arithmetic () =
  check_run "let a = 1 + 2 * 3" [ "7" ] ();
  check_run "let a = (1 + 2) * 3" [ "9" ] ();
  check_run "let a = 7 / 2" [ "3" ] ();
  check_run "let a = 0 - 7 / 2" [ "-3" ] ();
  check_run "let a = 1 - 2 - 3" [ "-4" ] ()

let test_append () =
  check_run "let a = [1; 2] @ [3; 4]" [ "[1; 2; 3; 4]" ] ();
  check_run "let a = [] @ [1]" [ "[1]" ] ();
  check_run "let a = [1] @ []" [ "[1]" ] ();
  check_run "let a = [] @ []" [ "[]" ] ();
  check_run "let a = [\"p\"] @ [\"q\"]" [ "[\"p\"; \"q\"]" ] ();
  check_run "let a = [[1]] @ [[2]]" [ "[[1]; [2]]" ] ();
  check_run "let a = [1] @ [2] @ [3]" [ "[1; 2; 3]" ] ();
  check_run "let a = [1] @ [2] @ [3] @ [4]" [ "[1; 2; 3; 4]" ] ();
  check_run "let x = 5\nlet a = [1] @ [x] @ [9]" [ "5"; "[1; 5; 9]" ] ();
  check_type_fails "let a = [1] @ [true]" ();
  check_type_fails "let a = 1 @ [2]" ()

let test_append_precedence () =
  check_run "let a = 0 :: [1] @ [2]" [ "[0; 1; 2]" ] ();
  check_run "let a = [1] @ [2] = [1; 2]" [ "true" ] ();
  check_run "let f l = l @ [9]\nlet a = [1] |> f"
    [ "<fun> : int list -> int list"; "[1; 9]" ]
    ();
  check_run "let a = [1 + 1] @ [2 * 2]" [ "[2; 4]" ] ()

let test_modulo () =
  check_run "let a = 7 % 3" [ "1" ] ();
  check_run "let a = 2 + 7 % 3" [ "3" ] ();
  check_run "let a = 7 % 3 * 2" [ "2" ] ();
  check_run "let even n = n % 2 = 0\nlet a = [x | x <- [1..10], even x]"
    [ "<fun> : int -> bool"; "[2; 4; 6; 8; 10]" ]
    ();
  check_eval_error "let a = 1 % 0" "Division by zero" ()

let test_unary_minus () =
  check_run "let a = -5" [ "-5" ] ();
  check_run "let x = 3\nlet a = -x" [ "3"; "-3" ] ();
  check_run "let x = 3\nlet a = -x * 2" [ "3"; "-6" ] ();
  check_run "let a = - -5" [ "5" ] ();
  check_run "let a = 1 - -2" [ "3" ] ();
  check_run "let a = -2 + 3" [ "1" ] ();
  check_run "let a = match -5 with -5 -> 1 | _ -> 0" [ "1" ] ()

let test_minus_spacing () =
  check_run "let a = 1 - 2" [ "-1" ] ();
  check_run "let a = 1 -2" [ "-1" ] ();
  check_run "let a = 1 --2" [ "1" ] ();
  check_run "let x = 5\nlet a = x -1" [ "5"; "4" ] ();
  check_run "let f = fun y -> y + 1\nlet a = f (-1)"
    [ "<fun> : int -> int"; "0" ]
    ()

let test_comparison () =
  check_run "let a = 1 < 2" [ "true" ] ();
  check_run "let a = 2 <= 2" [ "true" ] ();
  check_run "let a = 1 = 1" [ "true" ] ();
  check_run "let x = 3\nlet a = 0 <= x < 5" [ "3"; "true" ] ();
  check_run "let x = 9\nlet a = 0 <= x < 5" [ "9"; "false" ] ()

let test_equality () =
  check_run "let a = \"x\" = \"x\"" [ "true" ] ();
  check_run "let a = \"x\" = \"y\"" [ "false" ] ();
  check_run "let a = () = ()" [ "true" ] ();
  check_run "let a = [1; 2] = [1; 2]" [ "true" ] ();
  check_run "let a = [1; 2] = [1; 3]" [ "false" ] ();
  check_run "let a = [1] = [1; 2]" [ "false" ] ();
  check_run "let a = [[1]; [2]] = [[1]; [2]]" [ "true" ] ();
  check_run "let a = [] = []" [ "true" ] ()

let test_function_equality () =
  check_eval_error "let f = fun x -> x\nlet a = f = f"
    "Cannot compare functions" ();
  check_eval_error "let f = fun x -> x\nlet a = [f] = [f]"
    "Cannot compare functions" ()

let test_connectives () =
  check_run "let a = true && false" [ "false" ] ();
  check_run "let a = true || false" [ "true" ] ();
  check_run "let a = 1 <> 2" [ "true" ] ();
  check_run "let a = [1] <> [1]" [ "false" ] ();
  check_run "let a = true || false && false" [ "true" ] ();
  check_run "let a = 1 < 2 && 3 < 4" [ "true" ] ()

let test_short_circuit () =
  check_run "let a = false && 1 / 0 = 0" [ "false" ] ();
  check_run "let a = true || 1 / 0 = 0" [ "true" ] ();
  check_run "let a = true && 1 = 1" [ "true" ] ();
  check_run "let a = false || 1 = 2" [ "false" ] ();
  check_eval_error "let a = true && 1 / 0 = 0" "Division by zero" ()

let test_precedence () =
  check_run "let l = [2]\nlet a = 1 :: l = [1; 2]" [ "[2]"; "true" ] ();
  check_run "let a = 1 = 1 = 1" [ "true" ] ();
  check_run "let a = 1 = 1 = 2" [ "false" ] ();
  check_run
    "let rec len = fun l -> match l with [] -> 0 | x :: xs -> 1 + len xs\n\
     let a = [1; 2; 3] |> len > 0"
    [ "<fun> : 'a list -> int"; "true" ]
    ()

let test_polymorphism () =
  check_run "let a = let id = fun x -> x in if id true then id 1 else 0" [ "1" ]
    ();
  check_run
    "let a =\n\
    \  let rec len = fun l -> match l with [] -> 0 | x :: xs -> 1 + len xs in\n\
    \  len [1; 2] + len [\"a\"]"
    [ "3" ] ();
  check_run
    "let id = fun x -> x\nlet a = id 1\nlet b = id true\nlet c = id \"s\""
    [ "<fun> : 'a -> 'a"; "1"; "true"; "\"s\"" ]
    ();
  check_run "let e = []\nlet a = 1 :: e\nlet b = true :: e"
    [ "[]"; "[1]"; "[true]" ] ()

let test_monomorphic_uses () =
  check_type_error "let a = let f = fun x -> x + 1 in f true"
    "In f true: Type mismatch: int and bool" ();
  check_type_error "let f = fun x -> x + 1\nlet a = f true"
    "In f true: Type mismatch: int and bool" ();
  check_type_error "let a = fun f -> if f true then f 1 else 0"
    "In f 1: Type mismatch: bool and int" ()

let test_repeated_pattern_variable () =
  check_type_error "let a = match [1; 1] with x :: x -> x | _ -> 0"
    "x is bound twice in the same pattern" ();
  check_type_error "let a = match [1; 1] with x :: y :: x -> x | _ -> 0"
    "x is bound twice in the same pattern" ();
  check_run "let a = match [1; 2] with x :: y :: rest -> x + y | _ -> 0" [ "3" ]
    ()

let test_exhaustive_match () =
  check_run "let a = match [1] with [] -> 0 | x :: xs -> x" [ "1" ] ();
  check_run "let a = match true with true -> 1 | false -> 0" [ "1" ] ();
  check_run "let a = match 5 with 1 -> 0 | _ -> 9" [ "9" ] ();
  check_run "let a = match 5 with n -> n" [ "5" ] ();
  check_run
    "let a = match [1; 2] with [] -> 0 | x :: [] -> x | x :: y :: r -> x + y"
    [ "3" ] ();
  check_run
    "let a = match [true] with [] -> 0 | true :: r -> 1 | false :: r -> 2"
    [ "1" ] ();
  check_run "let a = match () with _ -> 1" [ "1" ] ()

let test_non_exhaustive_match () =
  check_type_error "let a = match 5 with 1 -> 0" "This match is not exhaustive"
    ();
  check_type_error "let a = match true with true -> 1"
    "This match is not exhaustive" ();
  check_type_error "let a = match [1] with [] -> 0"
    "This match is not exhaustive" ();
  check_type_error "let a = match [1] with x :: xs -> x"
    "This match is not exhaustive" ();
  check_type_error "let a = match [1] with [] -> 0 | x :: [] -> x"
    "This match is not exhaustive" ();
  check_type_error "let a = match \"s\" with \"a\" -> 1 | \"b\" -> 2"
    "This match is not exhaustive" ();
  check_type_error "let a = match [1] with [] -> 0 | x :: y :: r -> 2"
    "This match is not exhaustive" ();
  check_type_error "let a = match [true] with [] -> 0 | true :: r -> 1"
    "This match is not exhaustive" ()

let test_conditional () =
  check_run "let a = if true then 1 else 2" [ "1" ] ();
  check_run "let a = if 1 < 0 then 1 else 2" [ "2" ] ();
  check_run "let a = if false then 1 / 0 else 2" [ "2" ] ()

let test_let () =
  check_run "let a = let x = 2 in x * x" [ "4" ] ();
  check_run "let a = let x = 1 in let y = 2 in x + y" [ "3" ] ();
  check_run "let x = 1\nlet a = let x = 2 in x" [ "1"; "2" ] ()

let test_application () =
  check_run "let f = fun x -> x + 1\nlet a = f 2"
    [ "<fun> : int -> int"; "3" ]
    ();
  check_run "let f = fun x -> x\nlet a = f [1; 2]"
    [ "<fun> : 'a -> 'a"; "[1; 2]" ]
    ();
  check_run "let a = (fun x -> x * x) 4" [ "16" ] ()

let test_multi_argument () =
  check_run "let add a b = a + b\nlet r = add 1 2"
    [ "<fun> : int -> int -> int"; "3" ]
    ();
  check_run "let f a b c = a + b + c\nlet r = f 1 2 3"
    [ "<fun> : int -> int -> int -> int"; "6" ]
    ();
  check_run "let add a b = a + b\nlet inc = add 1\nlet r = inc 5"
    [ "<fun> : int -> int -> int"; "<fun> : int -> int"; "6" ]
    ();
  check_run "let a = let f x y = x * y in f 3 4" [ "12" ] ();
  check_run "let a = (fun x y -> x + y) 1 2" [ "3" ] ();
  check_run
    "let rec gcd a b = if b = 0 then a else gcd b (a - (a / b) * b)\n\
     let r = gcd 48 18"
    [ "<fun> : int -> int -> int"; "6" ]
    ();
  check_run
    "let a = let rec go n acc = if n = 0 then acc else go (n - 1) (acc + n) in \
     go 4 0"
    [ "10" ] ()

let test_closures () =
  check_run "let x = 1\nlet f = fun y -> x + y\nlet x = 100\nlet r = f 5"
    [ "1"; "<fun> : int -> int"; "100"; "6" ]
    ();
  check_run "let add = fun x -> fun y -> x + y\nlet a = add 1 2"
    [ "<fun> : int -> int -> int"; "3" ]
    ();
  check_run
    "let make = fun n -> fun x -> x + n\nlet inc = make 1\nlet a = inc 5"
    [ "<fun> : int -> int -> int"; "<fun> : int -> int"; "6" ]
    ();
  check_run
    "let x = 1\n\
     let twice = fun f -> fun v -> f (f v)\n\
     let g = fun y -> x + y\n\
     let a = twice g 0"
    [ "1"; "<fun> : ('a -> 'a) -> 'a -> 'a"; "<fun> : int -> int"; "2" ]
    ()

let test_pipe () =
  check_run "let f = fun x -> x + 1\nlet a = 5 |> f"
    [ "<fun> : int -> int"; "6" ]
    ();
  check_run
    "let f = fun x -> x + 1\nlet g = fun x -> x * 2\nlet a = 5 |> f |> g"
    [ "<fun> : int -> int"; "<fun> : int -> int"; "12" ]
    ()

let test_recursion () =
  check_run
    "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1)\n\
     let a = fact 5"
    [ "<fun> : int -> int"; "120" ]
    ();
  check_run
    "let rec len = fun l -> match l with [] -> 0 | x :: xs -> 1 + len xs\n\
     let a = len [1; 2; 3]"
    [ "<fun> : 'a list -> int"; "3" ]
    ()

let test_cons () =
  check_run "let a = 1 :: [2; 3]" [ "[1; 2; 3]" ] ();
  check_run "let a = 1 :: 2 :: []" [ "[1; 2]" ] ();
  check_run "let l = [2]\nlet a = 1 :: l\nlet b = l" [ "[2]"; "[1; 2]"; "[2]" ]
    ()

let test_match () =
  check_run "let a = match [1; 2] with [] -> 0 | x :: xs -> x" [ "1" ] ();
  check_run "let a = match [] with [] -> 0 | x :: xs -> x" [ "0" ] ();
  check_run "let a = match [1; 2; 3] with x :: y :: rest -> x + y | _ -> 0"
    [ "3" ] ();
  check_run "let a = match 1 with _ -> 0 | 1 -> 9" [ "0" ] ()

let test_runtime_errors () =
  check_eval_error "let a = 1 / 0" "Division by zero" ();
  check_eval_error "let a = match [1] with [] -> 0 | x :: xs -> 1 / 0"
    "Division by zero" ()

let test_type_errors () =
  check_type_error "let a = 1 + true" "Type mismatch: bool and int" ();
  check_type_error "let a = x" "Unbound variable x" ()

let test_algorithm_examples () =
  let last src = List.nth (run src) (List.length (run src) - 1) in
  Alcotest.(check string)
    "merge sort" "[1; 2; 3; 5; 5; 6; 9]"
    (last (read_file "../examples/msort.nd"));
  Alcotest.(check string)
    "six queens"
    "[[5; 3; 1; 6; 4; 2]; [4; 1; 5; 2; 6; 3]; [3; 6; 2; 5; 1; 4]; [2; 4; 6; 1; \
     3; 5]]"
    (last (read_file "../examples/queens.nd"));
  Alcotest.(check string)
    "knapsack" "7"
    (last (read_file "../examples/knapsack.nd"));
  Alcotest.(check string)
    "bellman ford" "[0; 2; 7; 4; -2]"
    (last (read_file "../examples/bellman_ford.nd"))

let test_examples () =
  check_run
    (read_file "../examples/gcd.nd")
    [ "<fun> : int -> int -> int"; "6" ]
    ();
  check_run
    (read_file "../examples/triples.nd")
    [
      "<fun> : int -> int list list";
      "[[3; 4; 5]; [6; 8; 10]; [5; 12; 13]; [9; 12; 15]; [8; 15; 17]; [12; 16; \
       20]]";
    ]
    ();
  check_run
    (read_file "../examples/primes.nd")
    [
      "<fun> : bool -> bool";
      "<fun> : ('a -> bool) -> 'a list -> bool";
      "<fun> : int -> int list";
      "[2; 3; 5; 7; 11; 13; 17; 19; 23; 29]";
    ]
    ();
  check_run
    (read_file "../examples/sort.nd")
    [ "<fun> : int list -> int list"; "[1; 2; 5; 5; 6; 9]" ]
    ()

let tests =
  [
    ("bindings", `Quick, test_bindings);
    ("layout", `Quick, test_layout);
    ("double semicolon", `Quick, test_double_semicolon);
    ("comments", `Quick, test_comments);
    ("range", `Quick, test_range);
    ("comprehension", `Quick, test_comprehension);
    ("nested generators", `Quick, test_nested_generators);
    ("generator patterns", `Quick, test_generator_patterns);
    ("comprehension guards", `Quick, test_comprehension_guards);
    ("comprehension let", `Quick, test_comprehension_let);
    ("comprehension programs", `Quick, test_comprehension_programs);
    ("comprehension body", `Quick, test_comprehension_body);
    ("comprehension types", `Quick, test_comprehension_types);
    ("tuple", `Quick, test_tuple);
    ("tuple equality", `Quick, test_tuple_equality);
    ("tuple types", `Quick, test_tuple_types);
    ("tuple patterns", `Quick, test_tuple_patterns);
    ("tuple exhaustiveness", `Quick, test_tuple_exhaustiveness);
    ("parenthesised patterns", `Quick, test_parenthesised_patterns);
    ("declarations", `Quick, test_declarations);
    ("declaration types", `Quick, test_declaration_types);
    ("recursive declarations", `Quick, test_recursive_declarations);
    ("declaration equality", `Quick, test_declaration_equality);
    ("declaration errors", `Quick, test_declaration_errors);
    ("printing", `Quick, test_printing);
    ("arithmetic", `Quick, test_arithmetic);
    ("append", `Quick, test_append);
    ("append precedence", `Quick, test_append_precedence);
    ("modulo", `Quick, test_modulo);
    ("unary minus", `Quick, test_unary_minus);
    ("minus spacing", `Quick, test_minus_spacing);
    ("comparison", `Quick, test_comparison);
    ("equality", `Quick, test_equality);
    ("function equality", `Quick, test_function_equality);
    ("connectives", `Quick, test_connectives);
    ("short circuit", `Quick, test_short_circuit);
    ("precedence", `Quick, test_precedence);
    ("polymorphism", `Quick, test_polymorphism);
    ("monomorphic uses", `Quick, test_monomorphic_uses);
    ("repeated pattern variable", `Quick, test_repeated_pattern_variable);
    ("exhaustive match", `Quick, test_exhaustive_match);
    ("non exhaustive match", `Quick, test_non_exhaustive_match);
    ("conditional", `Quick, test_conditional);
    ("let", `Quick, test_let);
    ("application", `Quick, test_application);
    ("multi argument", `Quick, test_multi_argument);
    ("closures", `Quick, test_closures);
    ("pipe", `Quick, test_pipe);
    ("recursion", `Quick, test_recursion);
    ("cons", `Quick, test_cons);
    ("match", `Quick, test_match);
    ("runtime errors", `Quick, test_runtime_errors);
    ("type errors", `Quick, test_type_errors);
    ("examples", `Quick, test_examples);
    ("algorithm examples", `Quick, test_algorithm_examples);
  ]
