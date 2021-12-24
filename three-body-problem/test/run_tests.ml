open OUnit2
open Three_body

let tests =
  "test suite three-body-problem" >::: [ ("placeholder" >:: fun _ -> assert_equal true true) ]


let run_test = plot 10.
