let () =
  Alcotest.run "Nomad" [
    "Parser", Test_parser.tests;
    "Eval", Test_eval.tests;
  ]
