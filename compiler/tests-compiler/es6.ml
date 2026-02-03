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
        f = x=>{var g = y=>(x + y | 0) + 7 | 0; return g;};
       runtime.caml_register_global(0, [0, f], "Test");
       return;})
     (globalThis);
    //end
    |}];
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
           for(;;){
            if(0 === b) return [0, a, 0];
            if(1 === b) return [0, a, 1];
            [a, b] = [(b - 1 | 0) - 1 | 0, (a - 1 | 0) - 1 | 0];
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
           for(;;){
            if(0 === b) return [0, a, 0];
            if(1 === b) return [0, a, 1];
            var c = (a - 1 | 0) - 1 | 0;
            a = (b - 1 | 0) - 1 | 0;
            b = c;
           }
          }],
         "Test");
       return;
      }
      (globalThis));
    //end
    |}]
