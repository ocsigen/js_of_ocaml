// Generated by js_of_ocaml
//# buildInfo:effects=false, kind=cmo, use-js-string=true, version=5.4.0+git-e2e1f3d

//# unitInfo: Provides: Test_dynlink
//# unitInfo: Requires: Stdlib
(function
  (globalThis){
   "use strict";
   var runtime = globalThis.jsoo_runtime;
   function caml_call1(f, a0){
    return (f.l >= 0 ? f.l : f.l = f.length) == 1
            ? f(a0)
            : runtime.caml_call_gen(f, [a0]);
   }
   runtime.jsoo_create_file
    ("/static/cmis/test_dynlink.cmi",
     "Caml1999I032\x84\x95\xa6\xbe\0\0\0q\0\0\0\x1a\0\0\0`\0\0\0Z\xa0,Test_dynlink\xa0\xb0\xa0!f\x01\x01\x0f\xd0\xc0\xc1@\xc0\xb3\x90\xa3$unitF@\x90@\x02\x05\xf5\xe1\0@\0\xfc\xc0\xb3\x90\xa3$unitF@\x90@\x02\x05\xf5\xe1\0@\0\xfd@\x02\x05\xf5\xe1\0@\0\xfe@\xb0\xc0/test_dynlink.mlCdh\xc0\x04\x02Cdi@@\xa1\x04\x19@@@\x84\x95\xa6\xbe\0\0\0j\0\0\0\x0f\0\0\0:\0\0\0.\xa0\xa0,Test_dynlink\x900!\xbb\xb6\xc1m\xb8\xe5\x9c\xd8w\xe1>[\xa8\xb3\xf3\xa0\xa0&Stdlib\x900\x85\xf8]\xdbG\xed\xc0\xc7W\x96W\xee\xe7@0-\xa0\xa08CamlinternalFormatBasics\x900\xce\xcf\xaf\xd5\xc0!G?^5\x8c\x96\xacu\x02\xa0@\x84\x95\xa6\xbe\0\0\0\x04\0\0\0\x02\0\0\0\x05\0\0\0\x05\xa0\x90@@");
   var
    global_data = runtime.caml_get_global_data(),
    Stdlib = global_data.Stdlib;
   caml_call1(Stdlib[46], "Dynlink OK");
   var
    cst_Test_dynlink_f_Ok = "Test_dynlink.f Ok",
    Test_dynlink =
      [0,
       function(_a_){return caml_call1(Stdlib[46], cst_Test_dynlink_f_Ok);}];
   runtime.caml_register_global(3, Test_dynlink, "Test_dynlink");
   return;
  }
  (globalThis));