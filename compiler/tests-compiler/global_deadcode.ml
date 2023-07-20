open Util

let%expect_test "" =
  let program =
    compile_and_parse
      {|
      |}
  in
  print_fun_decl program (Some "");
  [%expect
    {|
    
    //end |}]