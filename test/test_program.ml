(* End to end tests: source text in, printed lines out, through the parser, the
   checker and the evaluator in turn. *)

open Nomad

let run src =
  match Parser.run Parser.program src with
  | Error e -> Alcotest.fail ("Parse error: " ^ e)
  | Ok stmts ->
      Check.check_program stmts;
      Eval.run_program stmts

(* [src] runs and prints exactly [expected]. *)
let check_run src expected () =
  Alcotest.(check (list string)) "output" expected (run src)

(* [src] parses and checks, but fails while running. *)
let check_eval_error src expected () =
  match run src with
  | _ -> Alcotest.fail "expected an evaluation error"
  | exception Eval.EvaluationError e ->
      Alcotest.(check string) "error message" expected e

(* [src] parses, but does not check. *)
let check_type_error src expected () =
  match run src with
  | _ -> Alcotest.fail "expected a type error"
  | exception Subst.TypeError e ->
      Alcotest.(check string) "error message" expected e

let read_file path =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let test_bindings () =
  check_run "let x = 1" [ "1" ] ();
  check_run "let x = 1\nlet y = x + 1" [ "1"; "2" ] ();
  (* A binding shadows the earlier one of the same name. *)
  check_run "let x = 1\nlet x = 2\nlet y = x" [ "1"; "2"; "2" ] ()

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

let test_comparison () =
  check_run "let a = 1 < 2" [ "true" ] ();
  check_run "let a = 2 <= 2" [ "true" ] ();
  check_run "let a = 1 = 1" [ "true" ] ();
  (* A run of comparisons expands to a conjunction, so both halves must hold. *)
  check_run "let x = 3\nlet a = 0 <= x < 5" [ "3"; "true" ] ();
  check_run "let x = 9\nlet a = 0 <= x < 5" [ "9"; "false" ] ()

let test_conditional () =
  check_run "let a = if true then 1 else 2" [ "1" ] ();
  check_run "let a = if 1 < 0 then 1 else 2" [ "2" ] ();
  (* Only the branch that is taken runs, so the other may be undefined. *)
  check_run "let a = if false then 1 / 0 else 2" [ "2" ] ()

let test_let () =
  check_run "let a = let x = 2 in x * x" [ "4" ] ();
  check_run "let a = let x = 1 in let y = 2 in x + y" [ "3" ] ();
  (* An inner binding hides the outer one for the rest of the body. *)
  check_run "let x = 1\nlet a = let x = 2 in x" [ "1"; "2" ] ()

let test_application () =
  check_run "let f = fun x -> x + 1\nlet a = f 2" [ "3" ] ();
  check_run "let f = fun x -> x\nlet a = f [1; 2]" [ "[1; 2]" ] ();
  check_run "let a = (fun x -> x * x) 4" [ "16" ] ()

let test_pipe () =
  check_run "let f = fun x -> x + 1\nlet a = 5 |> f" [ "6" ] ();
  check_run
    "let f = fun x -> x + 1\nlet g = fun x -> x * 2\nlet a = 5 |> f |> g"
    [ "12" ] ()

let test_recursion () =
  check_run
    "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1)\n\
     let a = fact 5"
    [ "120" ] ();
  check_run
    "let rec len = fun l -> match l with [] -> 0 | x :: xs -> 1 + len xs\n\
     let a = len [1; 2; 3]"
    [ "3" ] ()

let test_cons () =
  check_run "let a = 1 :: [2; 3]" [ "[1; 2; 3]" ] ();
  check_run "let a = 1 :: 2 :: []" [ "[1; 2]" ] ();
  (* Consing builds a new list and leaves the old one alone. *)
  check_run "let l = [2]\nlet a = 1 :: l\nlet b = l" [ "[2]"; "[1; 2]"; "[2]" ]
    ()

let test_match () =
  check_run "let a = match [1; 2] with [] -> 0 | x :: xs -> x" [ "1" ] ();
  check_run "let a = match [] with [] -> 0 | x :: xs -> x" [ "0" ] ();
  check_run "let a = match [1; 2; 3] with x :: y :: rest -> x + y | _ -> 0"
    [ "3" ] ();
  (* The first case that matches wins, so order decides the answer. *)
  check_run "let a = match 1 with _ -> 0 | 1 -> 9" [ "0" ] ()

let test_runtime_errors () =
  check_eval_error "let a = 1 / 0" "Division by zero" ();
  check_eval_error "let a = match 5 with 1 -> 0" "Match failure" ()

let test_type_errors () =
  check_type_error "let a = 1 + true" "Type mismatch: bool and int" ();
  check_type_error "let a = x" "Unbound variable x" ()

let test_examples () =
  check_run (read_file "../examples/euclid.nd") [ "6" ] ();
  check_run (read_file "../examples/sort.nd") [ "[1; 2; 5; 5; 6; 9]" ] ()

let tests =
  [
    ("bindings", `Quick, test_bindings);
    ("printing", `Quick, test_printing);
    ("arithmetic", `Quick, test_arithmetic);
    ("comparison", `Quick, test_comparison);
    ("conditional", `Quick, test_conditional);
    ("let", `Quick, test_let);
    ("application", `Quick, test_application);
    ("pipe", `Quick, test_pipe);
    ("recursion", `Quick, test_recursion);
    ("cons", `Quick, test_cons);
    ("match", `Quick, test_match);
    ("runtime errors", `Quick, test_runtime_errors);
    ("type errors", `Quick, test_type_errors);
    ("examples", `Quick, test_examples);
  ]
