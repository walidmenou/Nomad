open Nomad.Ast
open Nomad.Eval

let check_eval env expr expected () =
  match eval env expr with
  | actual -> Alcotest.(check bool) "matches expected" true (actual = expected)
  | exception EvaluationError e -> Alcotest.fail ("EvaluationError: " ^ e)
  | exception e -> Alcotest.fail ("Unknown exception: " ^ Printexc.to_string e)

let check_error env expr expected () =
  match eval env expr with
  | _ -> Alcotest.fail "expected an evaluation error"
  | exception EvaluationError e ->
      Alcotest.(check string) "error message" expected e

let fact_body =
  Fun
    ( "n",
      If
        ( BinOp (Var "n", Equal, Int 0),
          Int 1,
          BinOp (Var "n", Mul, App (Var "fact", BinOp (Var "n", Sub, Int 1))) )
    )

let test_lit () =
  check_eval [] (Int 1) (VInt 1) ();
  check_eval [] (Bool true) (VBool true) ();
  check_eval [] (String "s") (VString "s") ();
  check_eval [] Unit VUnit ()

let test_var () =
  check_eval [ ("x", VInt 1) ] (Var "x") (VInt 1) ();
  check_eval [ ("x", VInt 2); ("x", VInt 1) ] (Var "x") (VInt 2) ()

let test_arith () =
  check_eval [] (BinOp (Int 1, Add, Int 2)) (VInt 3) ();
  check_eval [] (BinOp (Int 1, Sub, Int 2)) (VInt (-1)) ();
  check_eval [] (BinOp (Int 3, Mul, Int 4)) (VInt 12) ();
  check_eval [] (BinOp (Int 7, Div, Int 2)) (VInt 3) ();
  check_eval [] (BinOp (Int 7, Mod, Int 2)) (VInt 1) ();
  check_eval [] (BinOp (Int 6, Mod, Int 3)) (VInt 0) ();
  check_eval [] (BinOp (Int (-7), Mod, Int 3)) (VInt (-1)) ();
  check_eval [] (BinOp (Int 7, Mod, Int (-3))) (VInt 1) ();
  check_error [] (BinOp (Int 1, Div, Int 0)) "Division by zero" ();
  check_error [] (BinOp (Int 1, Mod, Int 0)) "Division by zero" ()

let test_compare () =
  check_eval [] (BinOp (Int 1, Less, Int 2)) (VBool true) ();
  check_eval [] (BinOp (Int 2, Leq, Int 2)) (VBool true) ();
  check_eval [] (BinOp (Int 1, Greater, Int 2)) (VBool false) ();
  check_eval [] (BinOp (Int 1, Equal, Int 1)) (VBool true) ();
  check_eval [] (BinOp (Bool true, And, Bool false)) (VBool false) ();
  check_eval [] (BinOp (Bool true, Or, Bool false)) (VBool true) ()

let test_equality () =
  check_eval [] (BinOp (String "a", Equal, String "a")) (VBool true) ();
  check_eval [] (BinOp (Unit, Equal, Unit)) (VBool true) ();
  check_eval [] (BinOp (List [ Int 1 ], Equal, List [ Int 1 ])) (VBool true) ();
  check_eval [] (BinOp (Int 1, Diff, Int 2)) (VBool true) ();
  check_eval [] (BinOp (String "a", Diff, String "a")) (VBool false) ();
  check_eval [] (BinOp (List [ Int 1 ], Diff, List [ Int 2 ])) (VBool true) ();
  check_error []
    (BinOp (Fun ("x", Var "x"), Equal, Fun ("x", Var "x")))
    "Cannot compare functions" ()

let test_if () =
  check_eval [] (If (Bool true, Int 1, Int 2)) (VInt 1) ();
  check_eval [] (If (Bool false, Int 1, Int 2)) (VInt 2) ();
  check_eval [] (If (Bool false, BinOp (Int 1, Div, Int 0), Int 2)) (VInt 2) ()

let test_let () =
  check_eval [] (Let ("x", Int 2, BinOp (Var "x", Mul, Var "x"))) (VInt 4) ();
  check_eval [ ("x", VInt 1) ] (Let ("x", Int 2, Var "x")) (VInt 2) ()

let test_app () =
  check_eval []
    (App (Fun ("x", BinOp (Var "x", Add, Int 1)), Int 2))
    (VInt 3) ();
  check_eval [] (App (Fun ("x", Var "x"), List [ Int 1 ])) (VList [ VInt 1 ]) ()

let test_closure () =
  let f = Fun ("y", BinOp (Var "x", Add, Var "y")) in
  check_eval []
    (Let ("x", Int 1, Let ("f", f, Let ("x", Int 100, App (Var "f", Int 5)))))
    (VInt 6) ();
  let add = Fun ("x", Fun ("y", BinOp (Var "x", Add, Var "y"))) in
  check_eval [] (App (App (add, Int 1), Int 2)) (VInt 3) ()

let test_rec () =
  check_eval [] (Rec ("fact", fact_body, App (Var "fact", Int 5))) (VInt 120) ()

let test_cons () =
  check_eval []
    (BinOp (Int 1, Cons, List [ Int 2 ]))
    (VList [ VInt 1; VInt 2 ])
    ();
  check_eval [] (BinOp (Int 1, Cons, List [])) (VList [ VInt 1 ]) ()

let test_match_failure () =
  check_error [] (Match (Int 5, [ (PatInt 1, Int 0) ])) "Match failure" ()

let test_list () =
  check_eval []
    (List [ Int 1; Int 2; Int 3 ])
    (VList [ VInt 1; VInt 2; VInt 3 ])
    ()

let test_match_wildcard () =
  check_eval [] (Match (Int 1, [ (PatWildcard, Int 2) ])) (VInt 2) ()

let test_match_var () =
  check_eval [] (Match (Int 1, [ (PatVar "x", Var "x") ])) (VInt 1) ()

let test_match_int () =
  check_eval []
    (Match (Int 1, [ (PatInt 1, Int 2); (PatWildcard, Int 3) ]))
    (VInt 2) ();
  check_eval []
    (Match (Int 4, [ (PatInt 1, Int 2); (PatWildcard, Int 3) ]))
    (VInt 3) ()

let test_match_bool () =
  check_eval []
    (Match (Bool true, [ (PatBool true, Int 2); (PatWildcard, Int 3) ]))
    (VInt 2) ()

let test_match_string () =
  check_eval []
    (Match (String "hello", [ (PatString "hello", Int 2); (PatWildcard, Int 3) ]))
    (VInt 2) ()

let test_match_shadowing () =
  check_eval
    [ ("x", VInt 1) ]
    (Match (Int 9, [ (PatVar "x", Var "x") ]))
    (VInt 9) ();
  check_eval
    [ ("y", VInt 7) ]
    (Match (Int 9, [ (PatVar "x", Var "y") ]))
    (VInt 7) ()

let tests =
  [
    ("literals", `Quick, test_lit);
    ("variables", `Quick, test_var);
    ("arithmetic", `Quick, test_arith);
    ("comparison", `Quick, test_compare);
    ("equality", `Quick, test_equality);
    ("if", `Quick, test_if);
    ("let", `Quick, test_let);
    ("application", `Quick, test_app);
    ("closures", `Quick, test_closure);
    ("let rec", `Quick, test_rec);
    ("cons", `Quick, test_cons);
    ("match failure", `Quick, test_match_failure);
    ("list", `Quick, test_list);
    ("match_wildcard", `Quick, test_match_wildcard);
    ("match_var", `Quick, test_match_var);
    ("match_int", `Quick, test_match_int);
    ("match_bool", `Quick, test_match_bool);
    ("match_string", `Quick, test_match_string);
    ("match_shadowing", `Quick, test_match_shadowing);
  ]
