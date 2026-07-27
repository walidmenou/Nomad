let () =
  Alcotest.run "Nomad" [
    "Parser", Test_parser.tests;
  ]
