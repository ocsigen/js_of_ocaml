let%expect_test _ =
  let prog =
    {|
let f x =
  let g y =
    let h z = x + y + z in
    h 7
  in
  g
    |}
  in
  let flags = [ "--enable"; "es6" ] in
  let program = Util.compile_and_parse ?effects:None ~pretty:true ~flags prog in
  Util.print_program program;
  [%expect
    {|
    (globalThis=>{
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        f = x=>{var g = y=>(x + y | 0) + 7 | 0; return g;},
        Test = [0, f];
       runtime.caml_register_global(0, Test, "Test");
       return;})
     (globalThis);
    //end |}];
  let program = Util.compile_and_parse ?effects:None ~pretty:false ~flags prog in
  Util.print_program program;
  [%expect
    {|
    (a=>{
       "use strict";
       var b = a.jsoo_runtime;
       b.caml_register_global(0, [0, b=>a=>(b + a | 0) + 7 | 0], "Test");
       return;})
     (globalThis);
    //end |}]

let%expect_test _ =
  let prog =
    {|
let rec odd n' = function
  0 -> n', false
| 1 -> n', true
| n -> odd (pred (pred n)) (pred (pred n'))
    |}
  in
  let flags = [ "--enable"; "es6" ] in
  let program = Util.compile_and_parse ?effects:None ~pretty:false ~flags prog in
  Util.print_program program;
  [%expect
    {|
    (a=>{
       "use strict";
       var b = a.jsoo_runtime;
       b.caml_register_global
        (0,
         [0,
          (a, b)=>{
           var d = a, c = b;
           for(;;){
            if(0 === c) return [0, d, 0];
            if(1 === c) return [0, d, 1];
            [d, c] = [(c - 1 | 0) - 1 | 0, (d - 1 | 0) - 1 | 0];
           }}],
         "Test");
       return;})
     (globalThis);
    //end
    |}];
  let program = Util.compile_and_parse ?effects:None ~pretty:false ~flags:[] prog in
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
            if(0 === c) return [0, d, 0];
            if(1 === c) return [0, d, 1];
            var e = (d - 1 | 0) - 1 | 0;
            d = (c - 1 | 0) - 1 | 0;
            c = e;
           }
          }],
         "Test");
       return;
      }
      (globalThis));
    //end |}]
