open Nomad.Ast
open Nomad.Parser

let check_parse p input expected () =
  match run p input with
  | Ok actual -> Alcotest.(check bool) "matches expected" true (actual = expected)
  | Error e -> Alcotest.fail e

let check_fail p input () =
  match run p input with
  | Ok _ -> Alcotest.fail "expected failure"
  | Error _ -> Alcotest.(check bool) "failed as expected" true true

let test_lit_expr () =
  check_parse lit_expr "123" (Int 123) ();
  check_parse lit_expr "-42" (Int (-42)) ();
  check_parse lit_expr "true" (Bool true) ();
  check_parse lit_expr "false" (Bool false) ();
  check_parse lit_expr "()" Unit ();
  check_parse lit_expr "\"hello\"" (String "hello") ()

let test_var_expr () =
  check_parse var_expr "x" (Var "x") ();
  check_parse var_expr "foo123" (Var "foo123") ();
  check_fail var_expr "1foo" ();
  check_fail var_expr "let" ()

let test_arith_expr () =
  check_parse expr "1 + 2" (BinOp (Int 1, Add, Int 2)) ();
  check_parse expr "1 * 2 + 3" (BinOp (BinOp (Int 1, Mul, Int 2), Add, Int 3)) ();
  check_parse expr "1 + 2 * 3" (BinOp (Int 1, Add, BinOp (Int 2, Mul, Int 3))) ()

let test_cmp_expr () =
  check_parse expr "1 < 2" (BinOp (Int 1, Less, Int 2)) ();
  check_parse expr "1 <= 2" (BinOp (Int 1, Leq, Int 2)) ();
  check_parse expr "1 < 2 < 3" (BinOp (BinOp (Int 1, Less, Int 2), And, BinOp (Int 2, Less, Int 3))) ()

let test_let_expr () =
  check_parse expr "let x = 1 in x" (Let ("x", Int 1, Var "x")) ()

let test_let_rec_expr () =
  check_parse expr "let rec f = fun x -> x in f" (Rec ("f", Fun ("x", Var "x"), Var "f")) ()

let test_if_expr () =
  check_parse expr "if true then 1 else 0" (If (Bool true, Int 1, Int 0)) ()

let test_fun_expr () =
  check_parse expr "fun x -> x" (Fun ("x", Var "x")) ()

let test_app_expr () =
  check_parse expr "f x" (App (Var "f", Var "x")) ();
  check_parse expr "f x y" (App (App (Var "f", Var "x"), Var "y")) ()

let test_pipe_expr () =
  check_parse expr "x |> f" (App (Var "f", Var "x")) ();
  check_parse expr "x |> f |> g" (App (Var "g", App (Var "f", Var "x"))) ()

let test_match_expr () =
  check_parse expr "match x with _ -> 1 | y -> 2" (Match (Var "x", [(PatWildcard, Int 1); (PatVar "y", Int 2)])) ()

let test_pattern () =
  check_parse pattern "_" PatWildcard ();
  check_parse pattern "x" (PatVar "x") ();
  check_parse pattern "123" (PatInt 123) ();
  check_parse pattern "true" (PatBool true) ();
  check_parse pattern "\"abc\"" (PatString "abc") ()

let test_statement () =
  check_parse statement "let x = 1" (LetStmt ("x", Int 1)) ();
  check_parse statement "let rec f = fun x -> x" (RecStmt ("f", Fun ("x", Var "x"))) ();
  check_parse statement "1 + 1" (ExprStmt (BinOp (Int 1, Add, Int 1))) ()

let test_program () =
  check_parse program "let x = 1 let y = 2 if true then x + y else 0" 
    [LetStmt ("x", Int 1); LetStmt ("y", Int 2); ExprStmt (If (Bool true, BinOp (Var "x", Add, Var "y"), Int 0))] ()

let tests = [
  "literals", `Quick, test_lit_expr;
  "variables", `Quick, test_var_expr;
  "arithmetic", `Quick, test_arith_expr;
  "comparisons", `Quick, test_cmp_expr;
  "let", `Quick, test_let_expr;
  "let rec", `Quick, test_let_rec_expr;
  "if", `Quick, test_if_expr;
  "functions", `Quick, test_fun_expr;
  "applications", `Quick, test_app_expr;
  "pipe", `Quick, test_pipe_expr;
  "match", `Quick, test_match_expr;
  "patterns", `Quick, test_pattern;
  "statements", `Quick, test_statement;
  "programs", `Quick, test_program;
]
