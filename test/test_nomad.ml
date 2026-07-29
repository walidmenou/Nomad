let () =
  Alcotest.run "Nomad"
    [
      ("Parser", Test_parser.tests);
      ("Eval", Test_eval.tests);
      ("Infer", Test_infer.tests);
      ("Check", Test_check.tests);
      ("Program", Test_program.tests);
    ]
