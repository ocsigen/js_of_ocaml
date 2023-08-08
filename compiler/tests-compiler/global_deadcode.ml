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
  (* Expect: compiles functor members used in the program *)
  print_fun_decl program (Some "_e_");
  [%expect {|
    function _e_(Ord){
     function height(param){if(! param) return 0; var h = param[4]; return h;}
     function create(l, v, r){
      if(l) var h = l[4], hl = h; else var hl = 0;
      if(r) var h$0 = r[4], hr = h$0; else var hr = 0;
      var _m_ = hr <= hl ? hl + 1 | 0 : hr + 1 | 0;
      return [0, l, v, r, _m_];
     }
     function bal(l, v, r){
      if(l) var h = l[4], hl = h; else var hl = 0;
      if(r) var h$0 = r[4], hr = h$0; else var hr = 0;
      if((hr + 2 | 0) < hl){
       if(! l) return invalid_arg(_b_);
       var lr = l[3], lv = l[2], ll = l[1], _h_ = height(lr);
       if(_h_ <= height(ll)) return create(ll, lv, create(lr, v, r));
       if(! lr) return invalid_arg(_a_);
       var lrr = lr[3], lrv = lr[2], lrl = lr[1], _i_ = create(lrr, v, r);
       return create(create(ll, lv, lrl), lrv, _i_);
      }
      if((hl + 2 | 0) >= hr){
       var _l_ = hr <= hl ? hl + 1 | 0 : hr + 1 | 0;
       return [0, l, v, r, _l_];
      }
      if(! r) return invalid_arg(_d_);
      var rr = r[3], rv = r[2], rl = r[1], _j_ = height(rl);
      if(_j_ <= height(rr)) return create(create(l, v, rl), rv, rr);
      if(! rl) return invalid_arg(_c_);
      var rlr = rl[3], rlv = rl[2], rll = rl[1], _k_ = create(rlr, rv, rr);
      return create(create(l, v, rll), rlv, _k_);
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
     var empty = 0;
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
     return [0,
             empty,
             sentinal,
             sentinal,
             add,
             singleton,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             sentinal,
             find];
    }
    //end |}]

let%expect_test "Substitutes unused fields with sentinal" =
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
  (* Expect second field in each triple to be replaced by a sentinal variable. *)
  print_fun_decl program (Some "f");
  [%expect {|
    function f(b, x){
     var t = b ? [0, 1, sentinal, x] : [0, 3, sentinal, 4], v = t[3], u = t[1];
     return [0, u, v];
    }
    //end |}];
  (* And that variable is defined as `undefined` *)
  print_var_decl program "sentinal";
  [%expect {|
    var sentinal = undefined;
    //end |}]
