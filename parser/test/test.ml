let test_parse_create_stmt () =
  let expect = "create stream s (id int, name string, flag bool)" in
  let result = Parser__Main.parse_str expect in
  let actual = Parser__Sql.stmt_to_str result in 
  Alcotest.(check string) "same string" expect actual

let test_parse_insert_stmt () =
  let expect = {|insert into s (id, name, flag) values ((0, "neo", true), (1, "mike", false))|} in
  let result = Parser__Main.parse_str expect in
  let actual = Parser__Sql.stmt_to_str result in 
  Alcotest.(check string) "same string" expect actual

let test_parse_select_stmt () =
  let expect = "select id, name, flag from s where id > 0 group by id" in
  let result = Parser__Main.parse_str expect in
  let actual = Parser__Sql.stmt_to_str result in 
  Alcotest.(check string) "same string" expect actual

let tests = [
    Alcotest.test_case "parse create stmt" `Quick test_parse_create_stmt;
    Alcotest.test_case "parse insert stmt" `Quick test_parse_insert_stmt;
    Alcotest.test_case "parse select stmt" `Quick test_parse_select_stmt;
]
