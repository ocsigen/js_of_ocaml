(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Tests for the use of OxCaml's "unyielding call" bytecode debug info by the
   double-translation effects backend. When the OCaml compiler has proven that
   a call cannot perform an effect (the function and all of its arguments are
   at mode [unyielding]), it records an [Event_unyielding_call] debug event
   (with [-g]), and js_of_ocaml may then call the direct-style version of the
   callee even in CPS context.

   These tests require an OxCaml compiler that emits these events. *)

[@@@if oxcaml]

open! Util

(* [local_effects] performs an effect but handles it itself, so calling it can
   never perform a free effect. The mode system knows this: [local_effects] is
   at mode [unyielding], so the call [f ()] in [apply_fn] carries an
   unyielding-call marker. The static analysis of the double translation is
   less precise: it considers that any call to [local_effects] may perform an
   effect. So, without the unyielding-call info, [apply_fn] would get
   double-translated and the call [f ()] compiled in CPS. *)
let code_unyielding =
  {|
         open Effect
         open Effect.Deep

         type _ Effect.t += E : unit Effect.t

         let local_effects () =
           Safe.try_with (fun h () -> Effect.Safe.perform h E; 1) ()
             { effc = (fun (type a) (eff : a Effect.t) ->
                 match eff with
                 | E -> Some (fun (k : (a, _) continuation) -> continue k ())
                 | _ -> None) }

         let apply_fn (f : unit -> int) =
           let r = ref 0 in
           for _ = 0 to 2 do incr r done; (* prevent inlining *)
           f () + !r

         let () = print_int (apply_fn local_effects + apply_fn (fun () -> 2))
|}

let%expect_test "unyielding calls stay direct in CPS context" =
  let program = compile_and_parse ~effects:`Double_translation code_unyielding in
  (* [apply_fn] is not double-translated: it calls the direct version of [f],
     even though [f] may be [local_effects], which is double-translated *)
  print_fun_decl program (Some "apply_fn");
  print_double_fun_decl program "apply_fn";
  [%expect
    {|
    function apply_fn(f){
     var r = 0, for$ = 0;
     for(;;){
      var r$0 = r + 1 | 0, _b_ = for$ + 1 | 0;
      if(2 === for$) return caml_call1(f, 0) + r$0 | 0;
      r = r$0;
      for$ = _b_;
     }
    }
    //end
    not found
    |}]

let%expect_test
    "unyielding calls are CPS-translated with --disable \
     oxcaml-use-unyielding-debuginfo-for-effect-cps" =
  let program =
    compile_and_parse
      ~effects:`Double_translation
      ~flags:[ "--disable"; "oxcaml-use-unyielding-debuginfo-for-effect-cps" ]
      code_unyielding
  in
  print_double_fun_decl program "apply_fn";
  [%expect
    {|
    function apply_fn$0(f){
     var r = 0, for$ = 0;
     for(;;){
      var r$0 = r + 1 | 0, _d_ = for$ + 1 | 0;
      if(2 === for$) return caml_call1(f, 0) + r$0 | 0;
      r = r$0;
      for$ = _d_;
     }
    }
    //end
    function apply_fn$1(f, cont){
     var for$ = 0, r = 0;
     for(;;){
      var r$0 = r + 1 | 0, _d_ = for$ + 1 | 0;
      if(2 === for$) break;
      for$ = _d_;
      r = r$0;
     }
     return caml_trampoline_cps_call2
             (f, 0, function(_d_){return cont(_d_ + r$0 | 0);});
    }
    //end
    var apply_fn = caml_cps_closure(apply_fn$0, apply_fn$1);
    //end
    |}]

(* Without [-g] there are no debug events, so the unyielding-call info cannot
   change anything: the generated code is identical with and without it. *)
let%expect_test "without debug info, the unyielding-call info changes nothing" =
  with_temp_dir ~f:(fun () ->
      let bc =
        code_unyielding
        |> Filetype.ocaml_text_of_string
        |> Filetype.write_ocaml ~name:"test.ml"
        |> compile_ocaml_to_bc ~debug:false
      in
      let js_text flags =
        let file =
          compile_bc_to_javascript ~flags ~effects:`Double_translation ~sourcemap:false bc
        in
        In_channel.with_open_bin (Filetype.path_of_js_file file) In_channel.input_all
      in
      let with_info = js_text [] in
      let without_info =
        js_text [ "--disable"; "oxcaml-use-unyielding-debuginfo-for-effect-cps" ]
      in
      print_endline
        (if String.equal with_info without_info then "identical" else "different"));
  [%expect {| identical |}]

(* A function only needs a CPS version if it may run below an effect handler.
   [call_yielding] calls its argument [g], which is at mode [yielding] and
   unknown to the compiler, so this call may perform an effect and is in CPS.
   But [call_yielding] is only reached through unyielding calls: it is called
   from [run], which is called from the fiber body; both calls are unyielding,
   since the functions and their arguments are unyielding. So [call_yielding]
   can never run below an effect handler, and it is compiled in direct style
   only. The functions are local: the toplevel values of a compilation unit
   are exported, hence escape, and may then be called from anywhere. *)
let code_below_handler =
  {|
    open Effect
    open Effect.Deep
    type _ Effect.t += E : unit Effect.t
    let l = ref []
    let fns = ref [ (fun () -> ()) ]
    let () =
      let call_yielding (g : (unit -> unit) @ yielding) =
        l := (fun () -> ()) :: !l; (* prevent inlining *)
        g ()
      in
      let run () =
        l := (fun () -> ()) :: !l; (* prevent inlining *)
        (* Two call sites, so that the functions do not get inlined; the
           argument is unknown to the compiler *)
        call_yielding (List.hd !fns);
        call_yielding (List.nth !fns 0)
      in
      Safe.try_with (fun _ () -> run (); run ()) ()
        { effc = (fun (type a) (eff : a Effect.t) ->
            match eff with
            | E -> Some (fun (k : (a, _) continuation) -> continue k ())
            | _ -> None) }
|}

let%expect_test
    "functions only reached through unyielding calls are not double-translated" =
  let program = compile_and_parse ~effects:`Double_translation code_below_handler in
  print_fun_decl program (Some "call_yielding");
  print_double_fun_decl program "call_yielding";
  print_double_fun_decl program "run";
  [%expect
    {|
           function call_yielding(g){
            l[1] = [0, function(param){return 0;}, l[1]];
            return caml_call1(g, 0);
           }
           //end
           not found
           not found
           |}]

let%expect_test
    "functions only reached through unyielding calls are double-translated with \
     --disable oxcaml-use-unyielding-debuginfo-for-effect-cps" =
  let program =
    compile_and_parse
      ~effects:`Double_translation
      ~flags:[ "--disable"; "oxcaml-use-unyielding-debuginfo-for-effect-cps" ]
      code_below_handler
  in
  print_double_fun_decl program "call_yielding";
  print_double_fun_decl program "run";
  [%expect
    {|
           function call_yielding$0(g){
            var _d_ = l[1];
            l[1] = [0, _b_(), _d_];
            return caml_call1(g, 0);
           }
           //end
           function call_yielding$1(g, cont){
            var _d_ = l[1];
            l[1] = [0, _b_(), _d_];
            return caml_trampoline_cps_call2(g, 0, cont);
           }
           //end
           var call_yielding = caml_cps_closure(call_yielding$0, call_yielding$1);
           //end
           function run$0(_d_){
            _d_ = l[1];
            l[1] = [0, _a_(), _d_];
            call_yielding(caml_call1(Stdlib_List[7], fns[1]));
            return call_yielding(caml_call2(Stdlib_List[9], fns[1], 0));
           }
           //end
           function run$1(_d_, cont){
            _d_ = l[1];
            l[1] = [0, _a_(), _d_];
            return caml_trampoline_cps_call2
                    (Stdlib_List[7],
                     fns[1],
                     function(_d_){
                      return caml_exact_trampoline_cps_call
                              (call_yielding,
                               _d_,
                               function(_d_){
                                return caml_trampoline_cps_call3
                                        (Stdlib_List[9],
                                         fns[1],
                                         0,
                                         function(_d_){
                                          return caml_exact_trampoline_cps_call
                                                  (call_yielding, _d_, cont);
                                         });
                               });
                     });
           }
           //end
           var run = caml_cps_closure(run$0, run$1);
           //end
           |}]

(* Calls to a function at mode [yielding] carry no unyielding-call marker and
   must remain in CPS: [h] may perform an effect. *)
let code_yielding =
  {|
    let l = ref []

    let call_twice (h : (unit -> unit) @ yielding) =
      l := (fun () -> ()) :: !l; (* prevent inlining *)
      h (); h ()

    let () =
      let open Effect in
      let open Effect.Deep in
      let module M = struct type _ Effect.t += E : unit Effect.t end in
      let counter = ref 0 in
      Safe.try_with
        (fun h () ->
          call_twice
            (fun () -> Effect.Safe.perform h M.E; incr counter)
            [@nontail]) ()
        { effc = (fun (type a) (eff : a Effect.t) ->
            match eff with
            | M.E -> Some (fun (k : (a, _) continuation) -> continue k ())
            | _ -> None) };
      print_int !counter
|}

let%expect_test "calls to yielding functions remain in CPS" =
  let program = compile_and_parse ~effects:`Double_translation code_yielding in
  print_double_fun_decl program "call_twice";
  [%expect
    {|
    function call_twice$0(h){
     var _c_ = l[1];
     l[1] = [0, _b_(), _c_];
     caml_call1(h, 0);
     return caml_call1(h, 0);
    }
    //end
    function call_twice$1(h, cont){
     var _c_ = l[1];
     l[1] = [0, _b_(), _c_];
     return caml_trampoline_cps_call2
             (h, 0, function(_c_){return caml_trampoline_cps_call2(h, 0, cont);});
    }
    //end
    var call_twice = caml_cps_closure(call_twice$0, call_twice$1);
    //end
    |}]

(* Runtime check: effects performed through a yielding closure still work with
   the unyielding-call info enabled, and unyielding code running under a
   handler computes the same results. *)
let code_run =
  {|
    type _ Effect.t += E : unit Effect.t

    let call_twice (f : (unit -> unit) @ yielding) = f (); f ()

    let f = Sys.opaque_identity (fun x -> x + 1)

    let rec loop n acc = if n = 0 then acc else loop (n - 1) (f acc)

    open Effect
    open Effect.Deep

    let handler =
      { effc = (fun (type a) (eff : a Effect.t) ->
            match eff with
            | E -> Some (fun (k : (a, _) continuation) -> continue k ())
            | _ -> None) }

    let () =
      let counter = ref 0 in
      Safe.try_with (fun h () ->
        call_twice (fun () -> Effect.Safe.perform h E; incr counter) [@nontail])
        ()
        handler;
      print_int !counter;
      print_newline ();
      let r = try_with (fun () -> loop 1000 0) () handler in
      print_int r;
      print_newline ()
|}

let%expect_test "performing effects still works" =
  compile_and_run ~effects:`Double_translation code_run;
  [%expect {|
    2
    1000
    |}]

let%expect_test
    "performing effects still works with --disable \
     oxcaml-use-unyielding-debuginfo-for-effect-cps" =
  compile_and_run
    ~effects:`Double_translation
    ~flags:[ "--disable"; "oxcaml-use-unyielding-debuginfo-for-effect-cps" ]
    code_run;
  [%expect {|
    2
    1000
    |}]

(* The return address of a non-tail call is the pc of the *next* instruction,
   which can itself be a call. In [(get ()) y] in tail position, the inner
   call [get ()] is compiled to an APPLY1 immediately followed by the APPTERM1
   of the outer tail call, so the inner call's across-call event -- carrying
   [Event_unyielding_call 1], since [get] and [()] are unyielding -- sits at
   exactly the pc of the APPTERM1, and both calls have one argument. That
   event must not be attributed to the outer tail call: [y] is yielding, so
   the tail call may perform an effect and must remain in CPS in the CPS
   version of [call_via_get]. *)
let code_nested_tail =
  {|
    type _ Effect.t += E : unit Effect.t

    let l = ref []

    let get () =
      l := (fun () -> ()) :: !l; (* prevent inlining *)
      Sys.opaque_identity (fun (h : (unit -> unit) @ yielding) -> h (); h ())

    let call_via_get (y : (unit -> unit) @ yielding) =
      l := (fun () -> ()) :: !l; (* prevent inlining *)
      (get ()) y

    open Effect
    open Effect.Deep

    (* Suspend on each perform and resume from outside the handler, so that
       the captured continuations are genuinely used across the call. *)
    let () =
      let saved = ref None in
      let counter = ref 0 in
      let r =
        Safe.try_with
          (fun h () ->
            call_via_get (fun () -> Effect.Safe.perform h E; incr counter)
            [@nontail];
            !counter)
          ()
          { effc = (fun (type a) (eff : a Effect.t) ->
              match eff with
              | E -> Some (fun (k : (a, _) continuation) ->
                  saved := Some (k : (unit, int) continuation); -1)
              | _ -> None) }
      in
      print_int r; print_newline ();
      let r = continue (Option.get !saved) () in
      print_int r; print_newline ();
      let r = continue (Option.get !saved) () in
      print_int r; print_newline ()
|}

let%expect_test
    "an inner call's unyielding info is not attributed to the tail call at its return \
     address" =
  let program = compile_and_parse ~effects:`Double_translation code_nested_tail in
  print_double_fun_decl program "call_via_get";
  [%expect
    {|
    function call_via_get$0(y){
     var _d_ = l[1];
     l[1] = [0, _c_(), _d_];
     return get()(y);
    }
    //end
    function call_via_get$1(y, cont){
     var _d_ = l[1];
     l[1] = [0, _c_(), _d_];
     return caml_exact_trampoline_cps_call(get(), y, cont);
    }
    //end
    var call_via_get = caml_cps_closure(call_via_get$0, call_via_get$1);
    //end
    |}]

let%expect_test "effects performed through the outer tail call still work" =
  compile_and_run ~effects:`Double_translation code_nested_tail;
  [%expect {|
    -1
    -1
    2
    |}]

(* The mirror image: the outer tail call is unyielding, the inner call is not.
   Here the tail call's pseudo marker and the inner call's across-call event
   would collide at the same pc; [Bytegen.merge_infos] resolves this by
   dropping the unyielding marker, so the inner (yielding) call cannot pick it
   up, and the outer tail call conservatively stays in CPS. *)
let code_nested_tail_unyielding_outer =
  {|
    type _ Effect.t += E : unit Effect.t

    let l = ref []

    let get (h : (unit -> unit) @ yielding) =
      l := (fun () -> ()) :: !l; (* prevent inlining *)
      h ();
      Sys.opaque_identity (fun (n : int) -> n + 1)

    let call_via_get (y : (unit -> unit) @ yielding) =
      l := (fun () -> ()) :: !l; (* prevent inlining *)
      (get y) 41

    open Effect
    open Effect.Deep

    let () =
      let counter = ref 0 in
      let r =
        Safe.try_with
          (fun h () ->
            call_via_get (fun () -> Effect.Safe.perform h E; incr counter)
            [@nontail]) ()
          { effc = (fun (type a) (eff : a Effect.t) ->
              match eff with
              | E -> Some (fun (k : (a, _) continuation) -> continue k ())
              | _ -> None) }
      in
      print_int !counter;
      print_newline ();
      print_int r
|}

let%expect_test
    "a tail-call marker colliding with an inner call's event is dropped by the compiler" =
  let program =
    compile_and_parse ~effects:`Double_translation code_nested_tail_unyielding_outer
  in
  print_double_fun_decl program "call_via_get";
  [%expect
    {|
    function call_via_get$0(y){
     var _e_ = l[1];
     l[1] = [0, _c_(), _e_];
     return get(y)(41);
    }
    //end
    function call_via_get$1(y, cont){
     var _e_ = l[1];
     l[1] = [0, _c_(), _e_];
     return caml_exact_trampoline_cps_call
             (get, y, function(_e_){return cont(_e_(41));});
    }
    //end
    var call_via_get = caml_cps_closure(call_via_get$0, call_via_get$1);
    //end
    |}]

let%expect_test "effects performed through the inner call still work" =
  compile_and_run ~effects:`Double_translation code_nested_tail_unyielding_outer;
  [%expect {|
    1
    42
    |}]
