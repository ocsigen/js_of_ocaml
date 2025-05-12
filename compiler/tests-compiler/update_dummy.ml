open Util

let%expect_test "deadcode elimination of cyclic values" =
  let program =
    compile_and_parse
      {|
      let f () =
        let rec x = 1 :: x in
        let rec y = 1 :: y in
        snd (x, y)
  |}
  in
  print_fun_decl program (Some "f");
  [%expect
    {|
    function f(param){
     var y = [];
     runtime.caml_update_dummy(y, [0, 1, y]);
     return y;
    }
    //end |}]
