open Nomad.Ast
open Nomad.Parser

let check_parse p input expected () =
  match run p input with
  | Ok actual ->
      Alcotest.(check bool) "matches expected" true (actual = expected)
  | Error e -> Alcotest.fail e

let check_fail p input () =
  match run p input with
  | Ok _ -> Alcotest.fail "expected failure"
  | Error _ -> Alcotest.(check bool) "failed as expected" true true

let test_lit_expr () =
  check_parse lit_expr "123" (Int 123) ();
  check_fail lit_expr "-42" ();
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
  check_parse expr "1 * 2 + 3"
    (BinOp (BinOp (Int 1, Mul, Int 2), Add, Int 3))
    ();
  check_parse expr "1 + 2 * 3"
    (BinOp (Int 1, Add, BinOp (Int 2, Mul, Int 3)))
    ()

let test_cmp_expr () =
  check_parse expr "1 < 2" (BinOp (Int 1, Less, Int 2)) ();
  check_parse expr "1 <= 2" (BinOp (Int 1, Leq, Int 2)) ();
  check_parse expr "1 < 2 < 3"
    (BinOp (BinOp (Int 1, Less, Int 2), And, BinOp (Int 2, Less, Int 3)))
    ()

let test_precedence () =
  check_parse expr "a || b && c"
    (BinOp (Var "a", Or, BinOp (Var "b", And, Var "c")))
    ();
  check_parse expr "a < b && c < d"
    (BinOp (BinOp (Var "a", Less, Var "b"), And, BinOp (Var "c", Less, Var "d")))
    ();
  check_parse expr "x :: xs = ys"
    (BinOp (BinOp (Var "x", Cons, Var "xs"), Equal, Var "ys"))
    ();
  check_parse expr "x |> f > 0"
    (BinOp (App (Var "f", Var "x"), Greater, Int 0))
    ();
  check_parse expr "a = b = c"
    (BinOp
       (BinOp (Var "a", Equal, Var "b"), And, BinOp (Var "b", Equal, Var "c")))
    ();
  check_parse expr "a <> b" (BinOp (Var "a", Diff, Var "b")) ()

let test_append () =
  check_parse expr "a @ b" (BinOp (Var "a", Append, Var "b")) ();
  check_parse expr "a @ b @ c"
    (BinOp (Var "a", Append, BinOp (Var "b", Append, Var "c")))
    ();
  check_parse expr "a @ b @ c @ d"
    (BinOp
       ( Var "a",
         Append,
         BinOp (Var "b", Append, BinOp (Var "c", Append, Var "d")) ))
    ();
  check_parse expr "x :: xs @ ys"
    (BinOp (BinOp (Var "x", Cons, Var "xs"), Append, Var "ys"))
    ();
  check_parse expr "a @ b = c"
    (BinOp (BinOp (Var "a", Append, Var "b"), Equal, Var "c"))
    ()

let test_modulo () =
  check_parse expr "7 % 3" (BinOp (Int 7, Mod, Int 3)) ();
  check_parse expr "2 + 7 % 3"
    (BinOp (Int 2, Add, BinOp (Int 7, Mod, Int 3)))
    ();
  check_parse expr "7 % 3 * 2"
    (BinOp (BinOp (Int 7, Mod, Int 3), Mul, Int 2))
    ();
  check_parse expr "10 % 4 % 3"
    (BinOp (BinOp (Int 10, Mod, Int 4), Mod, Int 3))
    ()

let test_unary_minus () =
  check_parse expr "-x" (BinOp (Int 0, Sub, Var "x")) ();
  check_parse expr "-x * y"
    (BinOp (BinOp (Int 0, Sub, Var "x"), Mul, Var "y"))
    ();
  check_parse expr "a - b" (BinOp (Var "a", Sub, Var "b")) ();
  check_parse expr "a -b" (BinOp (Var "a", Sub, Var "b")) ();
  check_parse pattern "-42" (PatInt (-42)) ()

let test_let_expr () =
  check_parse expr "let x = 1 in x" (Let ("x", Int 1, Var "x")) ()

let test_let_rec_expr () =
  check_parse expr "let rec f = fun x -> x in f"
    (Rec ("f", Fun ("x", Var "x"), Var "f"))
    ()

let test_if_expr () =
  check_parse expr "if true then 1 else 0" (If (Bool true, Int 1, Int 0)) ()

let test_fun_expr () =
  check_parse expr "fun x -> x" (Fun ("x", Var "x")) ();
  check_parse expr "fun x y -> x" (Fun ("x", Fun ("y", Var "x"))) ()

let test_multi_argument () =
  check_parse statement "let f x y = x"
    (LetStmt ("f", Fun ("x", Fun ("y", Var "x"))))
    ();
  check_parse statement "let rec f x = x" (RecStmt ("f", Fun ("x", Var "x"))) ();
  check_parse expr "let f x = x in f"
    (Let ("f", Fun ("x", Var "x"), Var "f"))
    ();
  check_parse expr "let rec f x = x in f"
    (Rec ("f", Fun ("x", Var "x"), Var "f"))
    ();
  check_parse statement "let x = 1" (LetStmt ("x", Int 1)) ()

let test_range () =
  check_parse expr "[1..5]" (Range (Int 1, Int 5)) ();
  check_parse expr "[a..b]" (Range (Var "a", Var "b")) ();
  check_parse expr "[1..n - 1]" (Range (Int 1, BinOp (Var "n", Sub, Int 1))) ();
  check_parse expr "[1; 2]" (List [ Int 1; Int 2 ]) ();
  check_parse expr "[]" (List []) ()

let test_comprehension () =
  check_parse expr "[x | x <- xs]"
    (Comp (Var "x", [ Gen (PatVar "x", Var "xs") ]))
    ();
  check_parse expr "[x + y | x <- xs, y <- ys]"
    (Comp
       ( BinOp (Var "x", Add, Var "y"),
         [ Gen (PatVar "x", Var "xs"); Gen (PatVar "y", Var "ys") ] ))
    ();
  check_parse expr "[x | h :: t <- xss]"
    (Comp (Var "x", [ Gen (PatCons (PatVar "h", PatVar "t"), Var "xss") ]))
    ();
  check_parse expr "[x | x <- [1..3]]"
    (Comp (Var "x", [ Gen (PatVar "x", Range (Int 1, Int 3)) ]))
    ();
  check_parse expr "[a < b]" (List [ BinOp (Var "a", Less, Var "b") ]) ()

let test_tuple () =
  check_parse expr "(1, 2)" (Tuple [ Int 1; Int 2 ]) ();
  check_parse expr "(1, 2, 3)" (Tuple [ Int 1; Int 2; Int 3 ]) ();
  check_parse expr "((1, 2), 3)" (Tuple [ Tuple [ Int 1; Int 2 ]; Int 3 ]) ();
  check_parse expr "(1)" (Int 1) ();
  check_parse expr "(1 + 2) * 3"
    (BinOp (BinOp (Int 1, Add, Int 2), Mul, Int 3))
    ();
  check_parse expr "()" Unit ()

let test_app_expr () =
  check_parse expr "f x" (App (Var "f", Var "x")) ();
  check_parse expr "f x y" (App (App (Var "f", Var "x"), Var "y")) ()

let test_pipe_expr () =
  check_parse expr "x |> f" (App (Var "f", Var "x")) ();
  check_parse expr "x |> f |> g" (App (Var "g", App (Var "f", Var "x"))) ()

let test_match_expr () =
  check_parse expr "match x with _ -> 1 | y -> 2"
    (Match (Var "x", [ (PatWildcard, Int 1); (PatVar "y", Int 2) ]))
    ()

let test_pattern () =
  check_parse pattern "_" PatWildcard ();
  check_parse pattern "x" (PatVar "x") ();
  check_parse pattern "123" (PatInt 123) ();
  check_parse pattern "true" (PatBool true) ();
  check_parse pattern "\"abc\"" (PatString "abc") ();
  check_parse expr "match s with \"a\" -> 1 | _ -> 0"
    (Match (Var "s", [ (PatString "a", Int 1); (PatWildcard, Int 0) ]))
    ()

let test_statement () =
  check_parse statement "let x = 1" (LetStmt ("x", Int 1)) ();
  check_parse statement "let rec f = fun x -> x"
    (RecStmt ("f", Fun ("x", Var "x")))
    ();
  check_parse statement "1 + 1" (ExprStmt (BinOp (Int 1, Add, Int 1))) ()

let test_type_stmt () =
  check_parse statement "type shape = Circle int | Rect int int"
    (TypeStmt
       ( "shape",
         [],
         [
           ("Circle", [ TECon ("int", []) ]);
           ("Rect", [ TECon ("int", []); TECon ("int", []) ]);
         ] ))
    ();
  check_parse statement "type 'a option = None | Some 'a"
    (TypeStmt ("option", [ "a" ], [ ("None", []); ("Some", [ TEVar "a" ]) ]))
    ();
  check_parse statement "type ('a, 'b) either = Left 'a | Right 'b"
    (TypeStmt
       ( "either",
         [ "a"; "b" ],
         [ ("Left", [ TEVar "a" ]); ("Right", [ TEVar "b" ]) ] ))
    ()

let test_type_expr () =
  check_parse type_expr "int" (TECon ("int", [])) ();
  check_parse type_expr "int list" (TECon ("list", [ TECon ("int", []) ])) ();
  check_parse type_expr "int option list"
    (TECon ("list", [ TECon ("option", [ TECon ("int", []) ]) ]))
    ();
  check_parse type_expr "(int, bool) either"
    (TECon ("either", [ TECon ("int", []); TECon ("bool", []) ]))
    ();
  check_parse type_expr "int * bool"
    (TETuple [ TECon ("int", []); TECon ("bool", []) ])
    ();
  check_parse type_expr "int -> bool -> int"
    (TEArrow (TECon ("int", []), TEArrow (TECon ("bool", []), TECon ("int", []))))
    ()

let test_program () =
  check_parse program "let x = 1 let y = 2 if true then x + y else 0"
    [
      LetStmt ("x", Int 1);
      LetStmt ("y", Int 2);
      ExprStmt (If (Bool true, BinOp (Var "x", Add, Var "y"), Int 0));
    ]
    ()

let tests =
  [
    ("literals", `Quick, test_lit_expr);
    ("variables", `Quick, test_var_expr);
    ("arithmetic", `Quick, test_arith_expr);
    ("comparisons", `Quick, test_cmp_expr);
    ("precedence", `Quick, test_precedence);
    ("append", `Quick, test_append);
    ("modulo", `Quick, test_modulo);
    ("unary minus", `Quick, test_unary_minus);
    ("let", `Quick, test_let_expr);
    ("let rec", `Quick, test_let_rec_expr);
    ("if", `Quick, test_if_expr);
    ("functions", `Quick, test_fun_expr);
    ("multi argument", `Quick, test_multi_argument);
    ("range", `Quick, test_range);
    ("tuple", `Quick, test_tuple);
    ("comprehension", `Quick, test_comprehension);
    ("applications", `Quick, test_app_expr);
    ("pipe", `Quick, test_pipe_expr);
    ("match", `Quick, test_match_expr);
    ("patterns", `Quick, test_pattern);
    ("statements", `Quick, test_statement);
    ("type declarations", `Quick, test_type_stmt);
    ("type expressions", `Quick, test_type_expr);
    ("programs", `Quick, test_program);
  ]
