open Nomad.Ast
open Nomad.Typecheck

let check_type env expr expected () =
  try
    let actual = infer env expr in
    Alcotest.(check bool) "matches expected" true (actual = expected)
  with TypeError e -> Alcotest.fail ("TypeError: " ^ e)

let check_fail env expr () =
  try
    let _ = infer env expr in
    Alcotest.fail "expected type error"
  with TypeError _ -> Alcotest.(check bool) "failed as expected" true true

let test_lit () =
  check_type [] (Int 42) TInt ();
  check_type [] (Bool true) TBool ();
  check_type [] (String "test") TString ();
  check_type [] Unit TUnit ()

let test_var () =
  check_type [("x", TInt)] (Var "x") TInt ();
  check_fail [] (Var "x") ()

let test_binop () =
  check_type [] (BinOp (Int 1, Add, Int 2)) TInt ();
  check_type [] (BinOp (Int 1, Less, Int 2)) TBool ();
  check_type [] (BinOp (Bool true, And, Bool false)) TBool ();
  check_fail [] (BinOp (Int 1, Add, Bool true)) ()

let test_if () =
  check_type [] (If (Bool true, Int 1, Int 2)) TInt ();
  check_fail [] (If (Int 1, Int 1, Int 2)) ();
  check_fail [] (If (Bool true, Int 1, Bool false)) ()

let test_fun () =
  (* fun x -> x  has type 'a -> 'a *)
  let t = infer [] (Fun ("x", Var "x")) in
  match t with
  | TArrow (TVar v1, TVar v2) when v1 = v2 -> Alcotest.(check bool) "identity func" true true
  | _ -> Alcotest.fail "Expected identity function type"

let test_app () =
  check_type [] (App (Fun ("x", BinOp (Var "x", Add, Int 1)), Int 2)) TInt ();
  check_fail [] (App (Fun ("x", BinOp (Var "x", Add, Int 1)), Bool true)) ()

let tests = [
  "literals", `Quick, test_lit;
  "variables", `Quick, test_var;
  "binop", `Quick, test_binop;
  "if", `Quick, test_if;
  "functions", `Quick, test_fun;
  "applications", `Quick, test_app;
]
