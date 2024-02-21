let%expect_test _ =
  let prog =
    {|
let rec f x y =
  match x,y with
  | 0, 0 -> true
  | _ -> f (y - 1) (x - 1)

|}
  in
  let program = Util.compile_and_parse ~pretty:false prog in
  Util.print_program program;
  [%expect
    {|
    (function(a){
       "use strict";
       var b = a.jsoo_runtime;
       b.caml_register_global
        (0,
         [0,
          function(a, b){
           var d = a, c = b;
           for(;;){
            if(0 === d && 0 === c) return 1;
            var e = d - 1 | 0, d = c - 1 | 0, c = e;
           }
          }],
         "Test");
       return;
      }
      (globalThis));
    //end |}]
