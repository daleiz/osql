(*
   Run all the OCaml test suites defined in the project.
*)

let test_suites: unit Alcotest.test list = [
  "osql_parser_test", Test_parser.Test.tests;
]

let () = Alcotest.run "osql" test_suites
