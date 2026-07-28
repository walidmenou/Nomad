open Nomad.Ast
open Nomad.Eval

let check_eval env expr expected () =
  match eval env expr with
  | actual -> Alcotest.(check bool) "matches expected" true (actual = expected)
  | exception EvaluationError e -> Alcotest.fail ("EvaluationError: " ^ e)
  | exception e -> Alcotest.fail ("Unknown exception: " ^ Printexc.to_string e)

let test_list () =
  check_eval [] (List [Int 1; Int 2; Int 3]) (List [Int 1; Int 2; Int 3]) ()

let test_match_wildcard () =
  check_eval [] (Match (Int 1, [(PatWildcard, Int 2)])) (Int 2) ()

let test_match_var () =
  check_eval [] (Match (Int 1, [(PatVar "x", Var "x")])) (Int 1) ()

let test_match_int () =
  check_eval [] (Match (Int 1, [(PatInt 1, Int 2); (PatWildcard, Int 3)])) (Int 2) ();
  check_eval [] (Match (Int 4, [(PatInt 1, Int 2); (PatWildcard, Int 3)])) (Int 3) ()

let test_match_bool () =
  check_eval [] (Match (Bool true, [(PatBool true, Int 2); (PatWildcard, Int 3)])) (Int 2) ()

let test_match_string () =
  check_eval [] (Match (String "hello", [(PatString "hello", Int 2); (PatWildcard, Int 3)])) (Int 2) ()

let tests = [
  "list", `Quick, test_list;
  "match_wildcard", `Quick, test_match_wildcard;
  "match_var", `Quick, test_match_var;
  "match_int", `Quick, test_match_int;
  "match_bool", `Quick, test_match_bool;
  "match_string", `Quick, test_match_string;
]
