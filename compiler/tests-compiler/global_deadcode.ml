open Util

let%expect_test "Eliminates unused functions from functor" =
  let program =
    (* Create two instances of Set functor so compiler can't inline one implementation. *)
    compile_and_parse_whole_program
    ~flags: ["--enable"; "globaldeadcode"]
      {|
      module Int_set = Set.Make (Int);;
      module String_set = Set.Make (String);;

      let int_set = Int_set.singleton 1 in
      let string_set = String_set.empty in
      let string_set = String_set.add "hello" string_set in
      print_endline (string_of_int (Int_set.find 1 int_set) ^ (String_set.find "hello" string_set))
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
       c = caml_call2(Ord[1], x, v);
      if(0 === c) return v;
      var r$0 = 0 <= c ? r : l, param$0 = r$0;
     }
    }
    //end |}];
  print_fun_decl program (Some "add");
  [%expect {|
    function add(x, t){
     if(! t) return [0, 0, x, 0, 1];
     var r = t[3], v = t[2], l = t[1], c = caml_call2(Ord[1], x, v);
     if(0 === c) return t;
     if(0 <= c){var rr = add(x, r); return r === rr ? t : bal(l, v, rr);}
     var ll = add(x, l);
     return l === ll ? t : bal(ll, v, r);
    }
    //end |}];
  (* Expect: does not compile unused functor member *)
  print_fun_decl program (Some "union");
  [%expect {| not found |}];
  print_fun_decl program (Some "inter");
  [%expect {| not found |}]

let%expect_test "Omit unused fields" =
  let program =
    compile_and_parse
    ~flags:["--enable"; "globaldeadcode"]
      {|
      let f b x =
        let t = if b then (1, 2, x) else (3, x, 4) in
        let (u, _, v) = t in
        (u, v)
      in print_int (fst (f true 1) + snd (f false 2))
      |}
  in 
  (* Expect second field in each triple to be omitted. *)
  print_fun_decl program (Some "f");
  [%expect {|
    function f(b, x){
     var t = b ? [0, 1, , x] : [0, 3, , 4], v = t[3], u = t[1];
     return [0, u, v];
    }
    //end |}]

let%expect_test "Omit unused return expressions" =
  let program =
    compile_and_parse
    ~flags:["--enable"; "globaldeadcode"]
      {|
      let f x =
        print_int x; 
        x + 1
      in 
      ignore (f 5 + f 6)
      |}
  in 
  (* Expect return value of f to be omitted. *)
  print_fun_decl program (Some "f");
  [%expect "
    function f(x){caml_call1(Stdlib[44], x); return;}
    //end"];