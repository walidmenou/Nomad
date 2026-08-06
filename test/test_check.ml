open Nomad.Ast
open Nomad.Subst
open Nomad.Check
open Nomad.Infer
open Nomad.Pretty

let bind env stmt = fst (check_stmt env stmt)

let check_stmts stmts expected () =
  match List.fold_left bind [] stmts with
  | env ->
      let binding (id, Forall (_, t)) = id ^ " : " ^ string_of_typ t in
      Alcotest.(check (list string))
        "bindings" expected (List.rev_map binding env)
  | exception TypeError e -> Alcotest.fail ("TypeError: " ^ e)

let check_error stmts expected () =
  match List.fold_left bind [] stmts with
  | _ -> Alcotest.fail "expected type error"
  | exception TypeError e -> Alcotest.(check string) "error message" expected e

let fact =
  Fun
    ( "n",
      If
        ( BinOp (Var "n", Equal, Int 0),
          Int 1,
          BinOp (Var "n", Mul, App (Var "f", BinOp (Var "n", Sub, Int 1))) ) )

let test_let () =
  check_stmts [ LetStmt (PatVar "x", Int 1) ] [ "x : int" ] ();
  check_stmts
    [
      LetStmt (PatVar "x", Int 1);
      LetStmt (PatVar "y", BinOp (Var "x", Add, Int 1));
    ]
    [ "x : int"; "y : int" ] ()

let test_rec () =
  check_stmts [ RecStmt ("f", fact) ] [ "f : (int -> int)" ] ();
  check_stmts
    [ RecStmt ("f", fact); LetStmt (PatVar "x", App (Var "f", Int 5)) ]
    [ "f : (int -> int)"; "x : int" ]
    ()

let test_expr () =
  check_stmts
    [ LetStmt (PatVar "x", Int 1); ExprStmt (BinOp (Var "x", Add, Int 1)) ]
    [ "x : int" ] ()

let test_program () =
  let types =
    check_program
      [
        LetStmt (PatVar "x", Int 1);
        RecStmt ("f", fact);
        ExprStmt (App (Var "f", Var "x"));
      ]
  in
  Alcotest.(check (list string))
    "statement types"
    [ "int"; "(int -> int)"; "int" ]
    (List.map string_of_typ types)

let test_errors () =
  check_error [ ExprStmt (Var "x") ] "Unbound variable x" ();
  check_error
    [
      LetStmt (PatVar "x", BinOp (Int 1, Add, Bool true));
      LetStmt (PatVar "y", Int 2);
    ]
    "Type mismatch: bool and int" ()

let tests =
  [
    ("let", `Quick, test_let);
    ("let rec", `Quick, test_rec);
    ("expression", `Quick, test_expr);
    ("program", `Quick, test_program);
    ("errors", `Quick, test_errors);
  ]
