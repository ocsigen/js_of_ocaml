// Generated by js_of_ocaml
//# buildInfo:effects=false, kind=cmo, use-js-string=true, version=5.7.1+a28514e

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
     'Caml1999I033\x84\x95\xa6\xbd\n`q\x1a`Z(\xb5/\xfd\0X\xbd\x02\0\xa4\x04\xa0,Test_dynlink\xa0\xb0\xa0!f\x01\x01\x10\xd0\xc0\xc1@\xc0\xb3\x90\xa3$unitF@\x90@\x02\x05\xf5\xe1\0@\0\xfc\xfd\xfe@\xb0\xc0/t.mlCdh\xc0\x04\x17Cdi@@\xa1\x04\x01@@@\x03\0R0\x15\x14q\x8f\xf4,\x03\x84\x95\xa6\xbe\0\0\0j\0\0\0\x0f\0\0\0:\0\0\0.\xa0\xa0,Test_dynlink\x9002\x98]\rl\xaf\xff\xf4\xd9\x07&\xa5g\x93\xc4\\\xa0\xa0&Stdlib\x900B\x1e\x91H\xdf\xf6\f\xb0\x1a\x86\xfe\xb57,\xe6\xa3\xa0\xa08CamlinternalFormatBasics\x900\xfbr\xa4&\x0eL\xc6\xc0\xd2"\xa5\xd6\x80\xc82x@\x84\x95\xa6\xbe\0\0\0\x04\0\0\0\x02\0\0\0\x05\0\0\0\x05\xa0\x90@@');
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
