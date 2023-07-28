open Util

let%expect_test "Eliminates unused functions from functor" =
  let program =
    compile_and_parse_whole_program
      {|
      module Int_set = Set.Make (Int);;

      let set = Int_set.singleton 1 in
      let set = Int_set.add 2 set in
      print_endline (string_of_int (Int_set.find 1 set + Int_set.find 2 set))
      |}
  in
  (* Expect: compiles functor members used in the program *)
  print_fun_decl program (Some "find");
  [%expect
    {|
    function find(x, param){
     var param$0 = param;
     for(;;){
      if(! param$0) throw caml_maybe_attach_backtrace(Not_found, 1);
      var
       r = param$0[3],
       v = param$0[2],
       l = param$0[1],
       c = caml_int_compare(x, v);
      if(0 === c) return v;
      var r$0 = 0 <= c ? r : l, param$0 = r$0;
     }
    }
    //end |}];
  print_fun_decl program (Some "add");
  [%expect {|
    function add(x, t){
     if(! t) return [0, 0, x, 0, 1];
     var r = t[3], v = t[2], l = t[1], c = caml_int_compare(x, v);
     if(0 === c) return t;
     if(0 <= c){var rr = add(x, r); return r === rr ? t : bal(l, v, rr);}
     var ll = add(x, l);
     return l === ll ? t : bal(ll, v, r);
    }
    //end |}];
  (* Expect: does not compile unused functor member *)
  print_fun_decl program (Some "union");
  [%expect {| not found |}]