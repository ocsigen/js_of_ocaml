(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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
open! Stdlib

let debug = Debug.find "main"

let times = Debug.find "times"

type optimized_result =
  { program : Code.program
  ; variable_uses : Deadcode.variable_uses
  ; trampolined_calls : Effects.trampolined_calls
  ; in_cps : Effects.in_cps
  ; deadcode_sentinal : Code.Var.t
  }

let should_export = function
  | `Iife -> false
  | `Named _ | `Anonymous -> true

let tailcall p =
  if debug () then Format.eprintf "Tail-call optimization...@.";
  Tailcall.f p

let deadcode' p =
  if debug () then Format.eprintf "Dead-code...@.";
  Deadcode.f p

let deadcode p =
  let p, _ = deadcode' p in
  let p = Deadcode.merge_blocks p in
  let p = Code.compact p in
  p

let inline p =
  if Config.Flag.inline () && Config.Flag.deadcode ()
  then (
    let p, live_vars = deadcode' p in
    if debug () then Format.eprintf "Inlining...@.";
    Inline.f p live_vars)
  else p

let specialize_1 (p, info) =
  if debug () then Format.eprintf "Specialize...@.";
  Specialize.f ~function_arity:(fun f -> Specialize.function_arity info f) p

let specialize_js (p, info) =
  if debug () then Format.eprintf "Specialize js...@.";
  Specialize_js.f info p

let specialize_js_once_before p =
  if debug () then Format.eprintf "Specialize js once...@.";
  Specialize_js.f_once_before p

let specialize_js_once_after p =
  if debug () then Format.eprintf "Specialize js once...@.";
  Specialize_js.f_once_after p

let specialize (p, info) =
  let p = specialize_1 (p, info) in
  let p = specialize_js (p, info) in
  p, info

let eval (p, info) = if Config.Flag.staticeval () then Eval.f info p, info else p, info

let flow p =
  if debug () then Format.eprintf "Data flow...@.";
  Flow.f p

let phi p =
  if debug () then Format.eprintf "Variable passing simplification...@.";
  Phisimpl.f p

let ( +> ) f g x = g (f x)

let map_fst f (x, y, z) = f x, y, z

let effects ~deadcode_sentinal p =
  match Config.effects () with
  | `Cps | `Double_translation ->
      if debug () then Format.eprintf "Effects...@.";
      let p, live_vars = Deadcode.f p in
      let info = Global_flow.f ~fast:false p in
      let p, live_vars =
        if Config.Flag.globaldeadcode ()
        then
          let p = Global_deadcode.f p ~deadcode_sentinal info in
          Deadcode.f p
        else p, live_vars
      in
      p
      |> Effects.f ~flow_info:info ~live_vars
      |> map_fst
           (match Config.target () with
           | `Wasm -> Fun.id
           | `JavaScript -> Lambda_lifting.f)
  | `Disabled | `Jspi ->
      ( p
      , (Code.Var.Set.empty : Effects.trampolined_calls)
      , (Code.Var.Set.empty : Effects.in_cps) )

let exact_calls (profile : Profile.t) ~deadcode_sentinal p =
  match Config.effects () with
  | `Disabled | `Jspi ->
      let fast =
        match profile with
        | O2 | O3 -> false
        | O1 -> true
      in
      let info = Global_flow.f ~fast p in
      let p =
        if Config.Flag.globaldeadcode () && Config.Flag.deadcode ()
        then Global_deadcode.f p ~deadcode_sentinal info
        else p
      in
      Specialize.f ~function_arity:(fun f -> Global_flow.function_arity info f) p
  | `Cps | `Double_translation -> p

let print p =
  if debug () then Code.Print.program Format.err_formatter (fun _ _ -> "") p;
  p

let stats = Debug.find "stats"

let rec loop max name round i (p : 'a) : 'a =
  let debug = times () || stats () in
  if debug then Format.eprintf "%s#%d...@." name i;
  let p' = round p in
  if i >= max
  then (
    if debug then Format.eprintf "%s#%d: couldn't reach fix point.@." name i;
    p')
  else if Code.equal p' p
  then (
    if debug then Format.eprintf "%s#%d: fix-point reached.@." name i;
    p')
  else loop max name round (i + 1) p'

let round : 'a -> 'a =
  print +> tailcall +> (flow +> specialize +> eval +> fst) +> inline +> phi +> deadcode

(* o1 *)

let o1 = loop 2 "round" round 1 +> (flow +> specialize +> eval +> fst) +> print

(* o2 *)

let o2 = loop 10 "round" round 1 +> print

(* o3 *)

let o3 = loop 30 "round" round 1 +> print

let generate
    ~exported_runtime
    ~wrap_with_fun
    ~warn_on_unhandled_effect
    { program; variable_uses; trampolined_calls; deadcode_sentinal; in_cps } =
  if times () then Format.eprintf "Start Generation...@.";
  let should_export = should_export wrap_with_fun in
  Generate.f
    program
    ~exported_runtime
    ~live_vars:variable_uses
    ~trampolined_calls
    ~in_cps
    ~should_export
    ~warn_on_unhandled_effect
    ~deadcode_sentinal

let debug_linker = Debug.find "linker"

let extra_js_files =
  lazy
    (List.fold_left (Builtins.all ()) ~init:[] ~f:(fun acc file ->
         try
           let name = Builtins.File.name file in
           let ss =
             List.concat_map
               ~f:Linker.Fragment.provides
               (Linker.Fragment.parse_builtin file)
             |> StringSet.of_list
           in
           (name, ss) :: acc
         with _ -> acc))

let report_missing_primitives missing =
  let missing =
    List.fold_left
      (Lazy.force extra_js_files)
      ~init:missing
      ~f:(fun missing (file, pro) ->
        let d = StringSet.inter missing pro in
        if not (StringSet.is_empty d)
        then (
          warn "Missing primitives provided by %s:@." file;
          StringSet.iter (fun nm -> warn "  %s@." nm) d;
          StringSet.diff missing pro)
        else missing)
  in
  if not (StringSet.is_empty missing)
  then (
    warn "Missing primitives:@.";
    StringSet.iter (fun nm -> warn "  %s@." nm) missing)

let gen_missing js missing =
  let open Javascript in
  let ident_s n = ident (Utf8_string.of_string_exn n) in
  let miss =
    StringSet.fold
      (fun prim acc ->
        let prim = Utf8_string.of_string_exn prim in
        let p = ident prim in
        ( p
        , ( ECond
              ( EBin
                  ( NotEqEq
                  , dot (EVar (ident Global_constant.global_object_)) prim
                  , EVar (ident_s "undefined") )
              , dot (EVar (ident Global_constant.global_object_)) prim
              , EFun
                  ( None
                  , fun_
                      []
                      [ ( Expression_statement
                            (call
                               (EVar (ident_s "caml_failwith"))
                               [ EBin
                                   ( Plus
                                   , EStr prim
                                   , EStr (Utf8_string.of_string_exn " not implemented")
                                   )
                               ]
                               N)
                        , N )
                      ]
                      N ) )
          , N ) )
        :: acc)
      missing
      []
  in
  if not (StringSet.is_empty missing)
  then (
    warn "There are some missing primitives@.";
    warn "Dummy implementations (raising 'Failure' exception) ";
    warn "will be used if they are not available at runtime.@.";
    warn "You can prevent the generation of dummy implementations with ";
    warn "the commandline option '--disable genprim'@.";
    report_missing_primitives missing);
  (variable_declaration miss, N) :: js

let mark_start_of_generated_code = Debug.find ~even_if_quiet:true "mark-runtime-gen"

let link' ~export_runtime ~standalone ~link (js : Javascript.statement_list) :
    Linker.output =
  if (not export_runtime) && not standalone
  then { runtime_code = js; always_required_codes = [] }
  else
    let check_missing = standalone in
    let t = Timer.make () in
    if times () then Format.eprintf "Start Linking...@.";
    let js =
      if mark_start_of_generated_code ()
      then
        let open Javascript in
        ( Expression_statement
            (EStr
               (Utf8_string.of_string_exn ("--MARK--" ^ "start-of-jsoo-gen" ^ "--MARK--")))
        , N )
        :: js
      else js
    in
    let used =
      let all_provided = Linker.list_all () in
      match link with
      | `All -> all_provided
      | `All_from from -> Linker.list_all ~from ()
      | `No -> StringSet.empty
      | `Needed ->
          let free = ref StringSet.empty in
          let o = new Js_traverse.fast_freevar (fun s -> free := StringSet.add s !free) in
          o#program js;
          let free = !free in
          let prim = Primitive.get_external () in
          let all_external = StringSet.union prim all_provided in
          StringSet.inter free all_external
    in
    let linkinfos =
      let from =
        match link with
        | `All_from l -> Some l
        | `All | `No | `Needed -> None
      in
      Linker.init ?from ()
    in
    let linkinfos, js =
      let linkinfos, missing = Linker.resolve_deps ~check_missing linkinfos used in
      (* gen_missing may use caml_failwith *)
      if (not (StringSet.is_empty missing)) && Config.Flag.genprim ()
      then
        let linkinfos, missing2 =
          Linker.resolve_deps
            ~check_missing
            linkinfos
            (StringSet.singleton "caml_failwith")
        in
        let missing = StringSet.union missing missing2 in
        linkinfos, gen_missing js missing
      else linkinfos, js
    in
    if times () then Format.eprintf "  linking: %a@." Timer.print t;
    let js =
      if export_runtime
      then
        let open Javascript in
        match Linker.all linkinfos with
        | [] -> js
        | all ->
            let all =
              List.map all ~f:(fun name ->
                  let name = Utf8_string.of_string_exn name in
                  Property (PNI name, EVar (ident name)))
            in
            (if standalone
             then
               ( Expression_statement
                   (EBin
                      ( Eq
                      , dot
                          (EVar (ident Global_constant.global_object_))
                          (Utf8_string.of_string_exn "jsoo_runtime")
                      , EObj all ))
               , N )
             else
               ( Expression_statement
                   (call
                      (dot
                         (EVar (ident (Utf8_string.of_string_exn "Object")))
                         (Utf8_string.of_string_exn "assign"))
                      [ dot
                          (EVar (ident Global_constant.global_object_))
                          (Utf8_string.of_string_exn "jsoo_runtime")
                      ; EObj all
                      ]
                      N)
               , N ))
            :: js
      else js
    in
    let missing = Linker.missing linkinfos in
    let output = Linker.link ~check_missing js linkinfos in
    if not (List.is_empty missing)
    then
      { output with
        runtime_code =
          (let open Javascript in
           ( Variable_statement
               ( Var
               , [ DeclPattern
                     ( ObjectBinding
                         { list =
                             List.map
                               ~f:(fun name ->
                                 let name = Utf8_string.of_string_exn name in
                                 Prop_ident (Prop_and_ident (ident name), None))
                               missing
                         ; rest = None
                         }
                     , ( dot
                           (EVar (ident Global_constant.global_object_))
                           (Utf8_string.of_string_exn "jsoo_runtime")
                       , N ) )
                 ] )
           , N )
           :: output.runtime_code)
      }
    else output

let check_js js =
  let t = Timer.make () in
  if times () then Format.eprintf "Start Checks...@.";
  let free = ref StringSet.empty in
  let o = new Js_traverse.fast_freevar (fun s -> free := StringSet.add s !free) in
  o#program js;
  let free = !free in
  let prim = Primitive.get_external () in
  let prov = Linker.list_all () in
  let all_external = StringSet.union prim prov in
  let missing = StringSet.inter free all_external in
  let missing = StringSet.diff missing Reserved.provided in
  let other = StringSet.diff free missing in
  if not (StringSet.is_empty missing) then report_missing_primitives missing;
  let probably_prov = StringSet.inter other Reserved.provided in
  let other = StringSet.diff other probably_prov in
  if (not (StringSet.is_empty other)) && debug_linker ()
  then (
    warn "Missing variables:@.";
    StringSet.iter (fun nm -> warn "  %s@." nm) other);
  if (not (StringSet.is_empty probably_prov)) && debug_linker ()
  then (
    warn "Variables provided by the browser:@.";
    StringSet.iter (fun nm -> warn "  %s@." nm) probably_prov);
  if times () then Format.eprintf "  checks: %a@." Timer.print t;
  js

let name_variables js =
  let t = Timer.make () in
  if times () then Format.eprintf "Start naming variables...@.";
  let js =
    if Config.Flag.shortvar ()
    then (
      let t5 = Timer.make () in
      let js = (new Js_traverse.rename_variable ~esm:false)#program js in
      if times () then Format.eprintf "    shortten vars: %a@." Timer.print t5;
      js)
    else js
  in
  let js = Js_assign.program js in
  if times () then Format.eprintf "  coloring: %a@." Timer.print t;
  js

let output formatter ~source_map () js =
  let t = Timer.make () in
  if times () then Format.eprintf "Start Writing file...@.";
  let sm = Js_output.program formatter ~source_map js in
  if times () then Format.eprintf "  write: %a@." Timer.print t;
  sm

let pack ~wrap_with_fun ~standalone { Linker.runtime_code = js; always_required_codes } =
  let module J = Javascript in
  let t = Timer.make () in
  if times () then Format.eprintf "Start Optimizing js...@.";
  (* pre pack optim *)
  let js =
    if Config.Flag.share_constant ()
    then (
      let t1 = Timer.make () in
      let js = Js_traverse.share_constant js in
      if times () then Format.eprintf "    share constant: %a@." Timer.print t1;
      js)
    else js
  in
  let js =
    if Config.Flag.compact_vardecl ()
    then (
      let t2 = Timer.make () in
      let js = (new Js_traverse.compact_vardecl)#program js in
      if times () then Format.eprintf "    compact var decl: %a@." Timer.print t2;
      js)
    else js
  in
  (* pack *)
  let wrap_in_iife ~use_strict js =
    let var ident e = J.variable_declaration [ J.ident ident, (e, J.N) ], J.N in
    let expr e = J.Expression_statement e, J.N in
    let free = ref StringSet.empty in
    let o = new Js_traverse.fast_freevar (fun s -> free := StringSet.add s !free) in
    o#program js;
    let freenames = !free in
    let export_shim js =
      if StringSet.mem Global_constant.exports freenames
      then
        if should_export wrap_with_fun
        then var Global_constant.exports_ (J.EObj []) :: js
        else
          let export_node =
            let s =
              Printf.sprintf
                {|((typeof module === 'object' && module.exports) || %s)|}
                Global_constant.global_object
            in
            let lex = Parse_js.Lexer.of_string s in
            Parse_js.parse_expr lex
          in
          var Global_constant.exports_ export_node :: js
      else js
    in
    let old_global_object_shim js =
      if StringSet.mem Global_constant.old_global_object freenames
      then
        var
          Global_constant.old_global_object_
          (J.EVar (J.ident Global_constant.global_object_))
        :: js
      else js
    in

    let efun args body = J.EFun (None, J.fun_ args body J.U) in
    let sfun name args body = J.Function_declaration (name, J.fun_ args body J.U), J.U in
    let mk f =
      let js = export_shim js in
      let js = old_global_object_shim js in
      let js =
        if use_strict
        then expr (J.EStr (Utf8_string.of_string_exn "use strict")) :: js
        else js
      in
      f [ J.ident Global_constant.global_object_ ] js
    in
    match wrap_with_fun with
    | `Anonymous -> expr (mk efun)
    | `Named name ->
        let name = Utf8_string.of_string_exn name in
        mk (sfun (J.ident name))
    | `Iife ->
        expr (J.call (mk efun) [ J.EVar (J.ident Global_constant.global_object_) ] J.N)
  in
  let always_required_js =
    (* consider adding a comments in the generated file with original
       location. e.g.
       {v
          //# 1 myfile.js
       v}
    *)
    List.map
      always_required_codes
      ~f:(fun { Linker.program; filename = _; requires = _ } ->
        wrap_in_iife ~use_strict:false program)
  in
  let runtime_js = wrap_in_iife ~use_strict:(Config.Flag.strictmode ()) js in
  let js = always_required_js @ [ runtime_js ] in
  let js =
    match wrap_with_fun, standalone with
    | `Named name, (true | false) ->
        assert (J.is_ident name);
        let export_node =
          let s =
            Printf.sprintf
              {|
if (typeof module === 'object' && module.exports) {
  module['exports'] = %s;
}
|}
              name
          in
          let lex = Parse_js.Lexer.of_string s in
          Parse_js.parse lex
        in
        js @ export_node
    | `Anonymous, _ -> js
    | `Iife, false -> js
    | `Iife, true ->
        let e =
          let s =
            {|
(function (Object) {
  typeof globalThis !== 'object' && (
    this ?
      get() :
      (Object.defineProperty(Object.prototype, '_T_', {
        configurable: true,
        get: get
      }), _T_)
  );
  function get() {
    var global = this || self;
    global.globalThis = global;
    delete Object.prototype._T_;
  }
}(Object));
|}
          in
          let lex = Parse_js.Lexer.of_string s in
          Parse_js.parse lex
        in
        e @ js
  in
  if times () then Format.eprintf "  packing: %a@." Timer.print t;
  js

let simplify_js js =
  (* post pack optim *)
  let t = Timer.make () in
  let t3 = Timer.make () in
  let js = (new Js_traverse.simpl)#program js in
  if times () then Format.eprintf "    simpl: %a@." Timer.print t3;
  let t4 = Timer.make () in
  let js = (new Js_traverse.clean)#program js in
  if times () then Format.eprintf "    clean: %a@." Timer.print t4;
  if times () then Format.eprintf "  optimizing: %a@." Timer.print t;
  js

let configure formatter =
  let pretty = Config.Flag.pretty () in
  Pretty_print.set_compact formatter (not pretty)

let link_and_pack ?(standalone = true) ?(wrap_with_fun = `Iife) ?(link = `No) p =
  let export_runtime =
    match link with
    | `All | `All_from _ -> true
    | `Needed | `No -> false
  in
  p
  |> link' ~export_runtime ~standalone ~link
  |> pack ~wrap_with_fun ~standalone
  |> check_js

let optimize ~profile p =
  let deadcode_sentinal =
    (* If deadcode is disabled, this field is just fresh variable *)
    Code.Var.fresh_n "dummy"
  in
  let opt =
    Specialize.switches
    +> specialize_js_once_before
    +> (match (profile : Profile.t) with
       | O1 -> o1
       | O2 -> o2
       | O3 -> o3)
    +> specialize_js_once_after
    +> exact_calls ~deadcode_sentinal profile
    +> effects ~deadcode_sentinal
    +> map_fst
         (match Config.target (), Config.effects () with
         | `JavaScript, `Disabled -> Generate_closure.f
         | `JavaScript, (`Cps | `Double_translation) | `Wasm, (`Jspi | `Cps) -> Fun.id
         | `JavaScript, `Jspi | `Wasm, (`Disabled | `Double_translation) -> assert false)
    +> map_fst deadcode'
  in
  if times () then Format.eprintf "Start Optimizing...@.";
  let t = Timer.make () in
  let (program, variable_uses), trampolined_calls, in_cps = opt p in
  let () = if times () then Format.eprintf " optimizations : %a@." Timer.print t in
  { program; variable_uses; trampolined_calls; in_cps; deadcode_sentinal }

let full ~standalone ~wrap_with_fun ~profile ~link ~source_map ~formatter p =
  let optimized_code = optimize ~profile p in
  let exported_runtime = not standalone in
  let emit formatter =
    generate ~exported_runtime ~wrap_with_fun ~warn_on_unhandled_effect:standalone
    +> link_and_pack ~standalone ~wrap_with_fun ~link
    +> simplify_js
    +> name_variables
    +> output formatter ~source_map ()
  in
  emit formatter optimized_code

let full_no_source_map ~formatter ~standalone ~wrap_with_fun ~profile ~link p =
  let (_ : Source_map.info) =
    full ~standalone ~wrap_with_fun ~profile ~link ~source_map:false ~formatter p
  in
  ()

let f
    ?(standalone = true)
    ?(wrap_with_fun = `Iife)
    ?(profile = Profile.O1)
    ~link
    ~source_map
    ~formatter
    p =
  full ~standalone ~wrap_with_fun ~profile ~link ~source_map ~formatter p

let f'
    ?(standalone = true)
    ?(wrap_with_fun = `Iife)
    ?(profile = Profile.O1)
    ~link
    formatter
    p =
  full_no_source_map ~formatter ~standalone ~wrap_with_fun ~profile ~link p

let from_string ~prims ~debug s formatter =
  let p = Parse_bytecode.from_string ~prims ~debug s in
  full_no_source_map
    ~formatter
    ~standalone:false
    ~wrap_with_fun:`Anonymous
    ~profile:O1
    ~link:`No
    p
