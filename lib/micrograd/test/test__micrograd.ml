open! Core

let%expect_test "hello" =
  print_s Micrograd.hello_world;
  [%expect {| "Hello, World!" |}]
;;
