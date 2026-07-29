open Nomad.Ast
open Nomad.Subst
open Nomad.Infer

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

let check_error env expr expected () =
  match infer env expr with
  | t -> Alcotest.fail ("expected type error, inferred " ^ string_of_typ t)
  | exception TypeError e -> Alcotest.(check string) "error message" expected e

let fact =
  Fun
    ( "n",
      If
        ( BinOp (Var "n", Equal, Int 0),
          Int 1,
          BinOp (Var "n", Mul, App (Var "f", BinOp (Var "n", Sub, Int 1))) ) )

let test_lit () =
  check_type [] (Int 42) TInt ();
  check_type [] (Bool true) TBool ();
  check_type [] (String "test") TString ();
  check_type [] Unit TUnit ()

let test_var () =
  check_type [ ("x", mono TInt) ] (Var "x") TInt ();
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
  let t = infer [] (Fun ("x", Var "x")) in
  match t with
  | TArrow (TVar v1, TVar v2) when v1 = v2 ->
      Alcotest.(check bool) "identity func" true true
  | _ -> Alcotest.fail "Expected identity function type"

let test_app () =
  check_type [] (App (Fun ("x", BinOp (Var "x", Add, Int 1)), Int 2)) TInt ();
  check_fail [] (App (Fun ("x", BinOp (Var "x", Add, Int 1)), Bool true)) ()

let test_let () =
  check_type [] (Let ("x", Int 1, BinOp (Var "x", Add, Int 2))) TInt ();
  check_type [] (Let ("f", Fun ("x", Var "x"), App (Var "f", Int 1))) TInt ()

let poly_use = If (App (Var "f", Bool true), App (Var "f", Int 1), Int 0)

let test_generalisation () =
  check_type [] (Let ("f", Fun ("x", Var "x"), poly_use)) TInt ();
  check_fail [] (Fun ("f", poly_use)) ();
  check_type []
    (Fun
       ( "y",
         Let
           ( "f",
             Fun ("x", Var "x"),
             BinOp (App (Var "f", Var "y"), Add, poly_use) ) ))
    (TArrow (TInt, TInt))
    ();
  check_type [] (Rec ("f", Fun ("x", Var "x"), poly_use)) TInt ()

let test_rec () =
  check_type [] (Rec ("f", fact, Var "f")) (TArrow (TInt, TInt)) ();
  check_type [] (Rec ("f", fact, App (Var "f", Int 5))) TInt ()

let test_list () =
  check_type [] (List [ Int 1; Int 2; Int 3 ]) (TList TInt) ();
  check_type [] (List [ List [ Int 1 ] ]) (TList (TList TInt)) ();
  check_fail [] (List [ Int 1; Bool true ]) ();
  match infer [] (List []) with
  | TList (TVar _) -> Alcotest.(check bool) "empty list" true true
  | t -> Alcotest.fail ("Expected 'a list, got " ^ string_of_typ t)

let test_cons () =
  check_type [] (BinOp (Int 1, Cons, List [ Int 2 ])) (TList TInt) ();
  check_type [] (BinOp (Int 1, Cons, List [])) (TList TInt) ();
  check_fail [] (BinOp (Int 1, Cons, List [ Bool true ])) ()

let test_match () =
  check_type []
    (Match (Int 1, [ (PatInt 1, Bool true); (PatWildcard, Bool false) ]))
    TBool ();
  check_type []
    (Match
       ( List [ Int 1 ],
         [ (PatNil, Int 0); (PatCons (PatVar "x", PatVar "xs"), Var "x") ] ))
    TInt ();
  check_fail []
    (Match (Int 1, [ (PatInt 1, Int 0); (PatWildcard, Bool true) ]))
    ()

let test_errors () =
  check_error [] (Var "x") "Unbound variable x" ();
  check_error []
    (BinOp (Int 1, Add, Bool true))
    "Type mismatch: bool and int" ();
  check_error []
    (If (Bool true, Int 1, Bool false))
    "Type mismatch: int and bool" ();
  check_error []
    (App (Fun ("x", BinOp (Var "x", Add, Int 1)), Bool true))
    "In (fun x -> x + 1) true: Type mismatch: int and bool" ();
  check_error []
    (Fun ("x", App (Var "x", Var "x")))
    "In x x: Recursive types not supported" ()

let test_printer () =
  let shows e expected =
    Alcotest.(check string) "printed" expected (string_of_expr e)
  in
  shows
    (Match
       ( Var "l",
         [ (PatNil, Int 0); (PatCons (PatVar "x", PatVar "xs"), Var "x") ] ))
    "match l with [] -> 0 | x :: xs -> x";
  shows (Let ("x", Int 1, BinOp (Var "x", Add, Int 2))) "let x = 1 in x + 2";
  shows
    (Rec ("f", Fun ("n", Var "n"), App (Var "f", Int 1)))
    "let rec f = fun n -> n in f 1";
  shows
    (If (BinOp (Var "n", Leq, Int 0), Bool true, Bool false))
    "if n <= 0 then true else false";
  shows (List [ Int 1; BinOp (Int 2, Mul, Int 3) ]) "[1; 2 * 3]";
  shows (BinOp (Int 1, Sub, BinOp (Int 2, Sub, Int 3))) "1 - (2 - 3)"

let tests =
  [
    ("literals", `Quick, test_lit);
    ("variables", `Quick, test_var);
    ("binop", `Quick, test_binop);
    ("if", `Quick, test_if);
    ("functions", `Quick, test_fun);
    ("applications", `Quick, test_app);
    ("let", `Quick, test_let);
    ("generalisation", `Quick, test_generalisation);
    ("let rec", `Quick, test_rec);
    ("lists", `Quick, test_list);
    ("cons", `Quick, test_cons);
    ("match", `Quick, test_match);
    ("errors", `Quick, test_errors);
    ("printer", `Quick, test_printer);
  ]
