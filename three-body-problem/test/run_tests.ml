open OUnit2

let tests =
  "test suite three-body-problem" >::: [ ("placeholder" >:: fun _ -> assert_equal true true) ]


let run_test = run_test_tt_main tests
