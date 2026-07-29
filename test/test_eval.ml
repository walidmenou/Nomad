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

(* fun n -> if n = 0 then 1 else n * fact (n - 1) *)
let fact_body =
  Fun
    ( "n",
      If
        ( BinOp (Var "n", Equal, Int 0),
          Int 1,
          BinOp (Var "n", Mul, App (Var "fact", BinOp (Var "n", Sub, Int 1))) )
    )

let test_lit () =
  check_eval [] (Int 1) (Int 1) ();
  check_eval [] (Bool true) (Bool true) ();
  check_eval [] (String "s") (String "s") ();
  check_eval [] Unit Unit ()

let test_var () =
  check_eval [ ("x", Int 1) ] (Var "x") (Int 1) ();
  (* The nearest binding of a name wins. *)
  check_eval [ ("x", Int 2); ("x", Int 1) ] (Var "x") (Int 2) ()

let test_arith () =
  check_eval [] (BinOp (Int 1, Add, Int 2)) (Int 3) ();
  check_eval [] (BinOp (Int 1, Sub, Int 2)) (Int (-1)) ();
  check_eval [] (BinOp (Int 3, Mul, Int 4)) (Int 12) ();
  (* Division truncates towards zero. *)
  check_eval [] (BinOp (Int 7, Div, Int 2)) (Int 3) ();
  check_error [] (BinOp (Int 1, Div, Int 0)) "Division by zero" ()

let test_compare () =
  check_eval [] (BinOp (Int 1, Less, Int 2)) (Bool true) ();
  check_eval [] (BinOp (Int 2, Leq, Int 2)) (Bool true) ();
  check_eval [] (BinOp (Int 1, Greater, Int 2)) (Bool false) ();
  check_eval [] (BinOp (Int 1, Equal, Int 1)) (Bool true) ();
  check_eval [] (BinOp (Bool true, And, Bool false)) (Bool false) ();
  check_eval [] (BinOp (Bool true, Or, Bool false)) (Bool true) ()

let test_if () =
  check_eval [] (If (Bool true, Int 1, Int 2)) (Int 1) ();
  check_eval [] (If (Bool false, Int 1, Int 2)) (Int 2) ();
  (* The branch that is not taken never runs. *)
  check_eval [] (If (Bool false, BinOp (Int 1, Div, Int 0), Int 2)) (Int 2) ()

let test_let () =
  check_eval [] (Let ("x", Int 2, BinOp (Var "x", Mul, Var "x"))) (Int 4) ();
  (* An inner binding hides the outer one. *)
  check_eval [ ("x", Int 1) ] (Let ("x", Int 2, Var "x")) (Int 2) ()

let test_app () =
  check_eval [] (App (Fun ("x", BinOp (Var "x", Add, Int 1)), Int 2)) (Int 3) ();
  check_eval [] (App (Fun ("x", Var "x"), List [ Int 1 ])) (List [ Int 1 ]) ()

let test_rec () =
  check_eval [] (Rec ("fact", fact_body, App (Var "fact", Int 5))) (Int 120) ()

let test_cons () =
  check_eval [] (BinOp (Int 1, Cons, List [ Int 2 ])) (List [ Int 1; Int 2 ]) ();
  check_eval [] (BinOp (Int 1, Cons, List [])) (List [ Int 1 ]) ()

let test_match_failure () =
  check_error [] (Match (Int 5, [ (PatInt 1, Int 0) ])) "Match failure" ()

let test_list () =
  check_eval [] (List [ Int 1; Int 2; Int 3 ]) (List [ Int 1; Int 2; Int 3 ]) ()

let test_match_wildcard () =
  check_eval [] (Match (Int 1, [ (PatWildcard, Int 2) ])) (Int 2) ()

let test_match_var () =
  check_eval [] (Match (Int 1, [ (PatVar "x", Var "x") ])) (Int 1) ()

let test_match_int () =
  check_eval []
    (Match (Int 1, [ (PatInt 1, Int 2); (PatWildcard, Int 3) ]))
    (Int 2) ();
  check_eval []
    (Match (Int 4, [ (PatInt 1, Int 2); (PatWildcard, Int 3) ]))
    (Int 3) ()

let test_match_bool () =
  check_eval []
    (Match (Bool true, [ (PatBool true, Int 2); (PatWildcard, Int 3) ]))
    (Int 2) ()

let test_match_string () =
  check_eval []
    (Match (String "hello", [ (PatString "hello", Int 2); (PatWildcard, Int 3) ]))
    (Int 2) ()

let tests =
  [
    ("literals", `Quick, test_lit);
    ("variables", `Quick, test_var);
    ("arithmetic", `Quick, test_arith);
    ("comparison", `Quick, test_compare);
    ("if", `Quick, test_if);
    ("let", `Quick, test_let);
    ("application", `Quick, test_app);
    ("let rec", `Quick, test_rec);
    ("cons", `Quick, test_cons);
    ("match failure", `Quick, test_match_failure);
    ("list", `Quick, test_list);
    ("match_wildcard", `Quick, test_match_wildcard);
    ("match_var", `Quick, test_match_var);
    ("match_int", `Quick, test_match_int);
    ("match_bool", `Quick, test_match_bool);
    ("match_string", `Quick, test_match_string);
  ]
