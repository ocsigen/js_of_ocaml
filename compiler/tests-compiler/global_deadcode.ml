open Util

let%expect_test "Eliminates unused functions from functor" =
  let program =
    (* Create two instances of Set functor so compiler can't inline one implementation. *)
    compile_and_parse_whole_program
      {|
      module Int_set = Set.Make (Int);;
      module String_set = Set.Make (String);;

      let int_set = Int_set.singleton 1 in
      let string_set = String_set.empty in
      let string_set = String_set.add "hello" string_set in
      print_endline (string_of_int (Int_set.find 1 int_set) ^ (String_set.find "hello" string_set))
       |}
  in
  let app = find_variable program "String_set" in
  (let open Js_of_ocaml_compiler in
   let open Stdlib in
   match app with
   | Javascript.ECall (EVar (S { name = Utf8 name; _ }), _, _, _) -> (
       let _, _, body, _ = find_function program name in
       match List.rev body with
       | (Return_statement (Some (EArr return), loc), loc') :: rest ->
           let return =
             ( Javascript.Return_statement
                 ( Some
                     (EArr
                        (List.filter return ~f:(function
                          | Javascript.ElementHole -> false
                          | _ -> true)))
                 , loc )
             , loc' )
           in
           print_program (List.rev (return :: rest))
       | _ -> ())
   | _ -> ());
  [%expect
    {|
    function height(param){if(! param) return 0; var h = param[4]; return h;}
    function create(l, v, r){
     if(l) var h = l[4], hl = h; else var hl = 0;
     if(r) var h$0 = r[4], hr = h$0; else var hr = 0;
     var _k_ = hr <= hl ? hl + 1 | 0 : hr + 1 | 0;
     return [0, l, v, r, _k_];
    }
    function bal(l, v, r){
     if(l) var h = l[4], hl = h; else var hl = 0;
     if(r) var h$0 = r[4], hr = h$0; else var hr = 0;
     if((hr + 2 | 0) < hl){
      if(! l) return invalid_arg(_b_);
      var lr = l[3], lv = l[2], ll = l[1], _f_ = height(lr);
      if(_f_ <= height(ll)) return create(ll, lv, create(lr, v, r));
      if(! lr) return invalid_arg(_a_);
      var lrr = lr[3], lrv = lr[2], lrl = lr[1], _g_ = create(lrr, v, r);
      return create(create(ll, lv, lrl), lrv, _g_);
     }
     if((hl + 2 | 0) >= hr){
      var _j_ = hr <= hl ? hl + 1 | 0 : hr + 1 | 0;
      return [0, l, v, r, _j_];
     }
     if(! r) return invalid_arg(_d_);
     var rr = r[3], rv = r[2], rl = r[1], _h_ = height(rl);
     if(_h_ <= height(rr)) return create(create(l, v, rl), rv, rr);
     if(! rl) return invalid_arg(_c_);
     var rlr = rl[3], rlv = rl[2], rll = rl[1], _i_ = create(rlr, rv, rr);
     return create(create(l, v, rll), rlv, _i_);
    }
    function add(x, t){
     if(! t) return [0, 0, x, 0, 1];
     var r = t[3], v = t[2], l = t[1], c = caml_call2(Ord[1], x, v);
     if(0 === c) return t;
     if(0 <= c){var rr = add(x, r); return r === rr ? t : bal(l, v, rr);}
     var ll = add(x, l);
     return l === ll ? t : bal(ll, v, r);
    }
    function singleton(x){return [0, 0, x, 0, 1];}
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
      var r$0 = 0 <= c ? r : l;
      param$0 = r$0;
     }
    }
    return [0, 0, add, singleton, find];
    //end |}]

let%expect_test "Omit unused fields" =
  let program =
    compile_and_parse
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
  [%expect
    {|
    function f(b, x){
     var t = b ? [0, 1, , x] : [0, 3, , 4], v = t[3], u = t[1];
     return [0, u, v];
    }
    //end |}]

let%expect_test "Omit unused return expressions" =
  let program =
    compile_and_parse
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
  [%expect {|
       function f(x){caml_call1(Stdlib[44], x);}
       //end
     |}]

let%expect_test "Bug fix in PR #1681" =
  let program =
    compile_and_parse_whole_program
      {|
      type t = {mutable a : int; b : int};;
      let f b =
        let x = {a = 1; b = 2} in
        if b then (
          x
        ) else (
          x.a <- 1; (* This has to be handled after [x] is returned *)
          {a = 3; b = 4}
        )
      let g = ref (fun _ -> assert false)
      let _ =
        (* We should not track that [f] is used below *)
        g := f; prerr_int ((!g true).b + (!g false).b)
      |}
  in
  print_fun_decl program (Some "f");
  (* No field of record x should be eliminated. *)
  [%expect
    {|
       function f(b){var x = [0, 1, 2]; return b ? x : (x[1] = 1, [0, 3, 4]);}
       //end
     |}]
