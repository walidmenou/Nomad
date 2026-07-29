open Nomad

let run src =
  match Parser.run Parser.program src with
  | Error e -> Alcotest.fail ("Parse error: " ^ e)
  | Ok stmts ->
      Check.check_program stmts;
      Eval.run_program stmts

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
  check_run "let id = fun x -> x\nid 1\nid true" [ "<fun>"; "1"; "true" ] ();
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
    [ "<fun>"; "true" ] ()

let test_conditional () =
  check_run "let a = if true then 1 else 2" [ "1" ] ();
  check_run "let a = if 1 < 0 then 1 else 2" [ "2" ] ();
  check_run "let a = if false then 1 / 0 else 2" [ "2" ] ()

let test_let () =
  check_run "let a = let x = 2 in x * x" [ "4" ] ();
  check_run "let a = let x = 1 in let y = 2 in x + y" [ "3" ] ();
  check_run "let x = 1\nlet a = let x = 2 in x" [ "1"; "2" ] ()

let test_application () =
  check_run "let f = fun x -> x + 1\nlet a = f 2" [ "<fun>"; "3" ] ();
  check_run "let f = fun x -> x\nlet a = f [1; 2]" [ "<fun>"; "[1; 2]" ] ();
  check_run "let a = (fun x -> x * x) 4" [ "16" ] ()

let test_closures () =
  check_run "let x = 1\nlet f = fun y -> x + y\nlet x = 100\nlet r = f 5"
    [ "1"; "<fun>"; "100"; "6" ]
    ();
  check_run "let add = fun x -> fun y -> x + y\nlet a = add 1 2"
    [ "<fun>"; "3" ] ();
  check_run
    "let make = fun n -> fun x -> x + n\nlet inc = make 1\nlet a = inc 5"
    [ "<fun>"; "<fun>"; "6" ] ();
  check_run
    "let x = 1\n\
     let twice = fun f -> fun v -> f (f v)\n\
     let g = fun y -> x + y\n\
     let a = twice g 0"
    [ "1"; "<fun>"; "<fun>"; "2" ]
    ()

let test_pipe () =
  check_run "let f = fun x -> x + 1\nlet a = 5 |> f" [ "<fun>"; "6" ] ();
  check_run
    "let f = fun x -> x + 1\nlet g = fun x -> x * 2\nlet a = 5 |> f |> g"
    [ "<fun>"; "<fun>"; "12" ] ()

let test_recursion () =
  check_run
    "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1)\n\
     let a = fact 5"
    [ "<fun>"; "120" ] ();
  check_run
    "let rec len = fun l -> match l with [] -> 0 | x :: xs -> 1 + len xs\n\
     let a = len [1; 2; 3]"
    [ "<fun>"; "3" ] ()

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
  check_eval_error "let a = match 5 with 1 -> 0" "Match failure" ()

let test_type_errors () =
  check_type_error "let a = 1 + true" "Type mismatch: bool and int" ();
  check_type_error "let a = x" "Unbound variable x" ()

let test_examples () =
  check_run (read_file "../examples/gcd.nd") [ "<fun>"; "6" ] ();
  check_run
    (read_file "../examples/sort.nd")
    [ "<fun>"; "<fun>"; "[1; 2; 5; 5; 6; 9]" ]
    ()

let tests =
  [
    ("bindings", `Quick, test_bindings);
    ("layout", `Quick, test_layout);
    ("double semicolon", `Quick, test_double_semicolon);
    ("comments", `Quick, test_comments);
    ("printing", `Quick, test_printing);
    ("arithmetic", `Quick, test_arithmetic);
    ("comparison", `Quick, test_comparison);
    ("equality", `Quick, test_equality);
    ("function equality", `Quick, test_function_equality);
    ("connectives", `Quick, test_connectives);
    ("short circuit", `Quick, test_short_circuit);
    ("precedence", `Quick, test_precedence);
    ("conditional", `Quick, test_conditional);
    ("let", `Quick, test_let);
    ("application", `Quick, test_application);
    ("closures", `Quick, test_closures);
    ("pipe", `Quick, test_pipe);
    ("recursion", `Quick, test_recursion);
    ("cons", `Quick, test_cons);
    ("match", `Quick, test_match);
    ("runtime errors", `Quick, test_runtime_errors);
    ("type errors", `Quick, test_type_errors);
    ("examples", `Quick, test_examples);
  ]
