
//# unitInfo: Provides: Shapes
//# shape: Shapes:[]
(function
  (globalThis){
   "use strict";
   var runtime = globalThis.jsoo_runtime;
   runtime.caml_register_global(0, [0], "Shapes");
   return;
  }
  (globalThis));

//# unitInfo: Provides: Shapes__M1
//# shape: Shapes__M1:[F(2)*]
(function
  (globalThis){
   "use strict";
   var runtime = globalThis.jsoo_runtime;
   function f(a, param){
     /*<<compiler/tests-full/m1.ml:1:14>>*/ return 0;
    /*<<compiler/tests-full/m1.ml:1:16>>*/ }
    /*<<?>>*/ runtime.caml_register_global(0, [0, f], "Shapes__M1");
   return;
  }
  (globalThis));

//# unitInfo: Provides: Shapes__M2
//# unitInfo: Requires: Stdlib
//# shape: Shapes__M2:[F(1)]
(function
  (globalThis){
   "use strict";
   var
    runtime = globalThis.jsoo_runtime,
    Stdlib = runtime.caml_get_global_data().Stdlib;
   function f(param){
     /*<<compiler/tests-full/m2.ml:1:11>>*/ return Stdlib[46].call(null, "") /*<<compiler/tests-full/m2.ml:1:27>>*/ ;
   }
    /*<<?>>*/ runtime.caml_register_global(2, [0, f], "Shapes__M2");
   return;
  }
  (globalThis));

//# unitInfo: Provides: Shapes__M3
//# unitInfo: Requires: Shapes__M1, Shapes__M2, Stdlib__Random
//# shape: Shapes__M3:[F(1)->F(2),N]
(function
  (globalThis){
   "use strict";
   var runtime = globalThis.jsoo_runtime;
   function caml_call2(f, a0, a1){
    return (f.l >= 0 ? f.l : f.l = f.length) === 2
            ? f(a0, a1)
            : runtime.caml_call_gen(f, [a0, a1]);
   }
   var
    global_data = runtime.caml_get_global_data(),
    Shapes_M2 = global_data.Shapes__M2,
    Stdlib_Random = global_data.Stdlib__Random,
    Shapes_M1 = global_data.Shapes__M1;
   function f(param){
     /*<<compiler/tests-full/m3.ml:1:14>>*/ return 1
            < Stdlib_Random[5].call(null, 2)
            ? Shapes_M1[1]
            : function
             (a, param){
               /*<<compiler/tests-full/m3.ml:1:59>>*/ return Shapes_M2[1].call
                      (null, 0) /*<<compiler/tests-full/m3.ml:1:66>>*/ ;
             };
   }
   var x =  /*<<compiler/tests-full/m3.ml:3:8>>*/ caml_call2(f(0), 0, 0);
    /*<<compiler/tests-full/m3.ml:3:18>>*/ runtime.caml_register_global
    (3, [0, f, x], "Shapes__M3");
   return;
   /*<<?>>*/ }
  (globalThis));
