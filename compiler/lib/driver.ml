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

type profile =
  | O1
  | O2
  | O3

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
  let r, _ = deadcode' p in
  r

let inline ~target p =
  if Config.Flag.inline () && Config.Flag.deadcode ()
  then (
    let p, live_vars = deadcode' p in
    if debug () then Format.eprintf "Inlining...@.";
    Inline.f ~target p live_vars)
  else p

let specialize_1 (p, info) =
  if debug () then Format.eprintf "Specialize...@.";
  Specialize.f ~function_arity:(fun f -> Specialize.function_arity info f) p

let specialize_js ~target (p, info) =
  if debug () then Format.eprintf "Specialize js...@.";
  Specialize_js.f ~target info p

let specialize_js_once p =
  if debug () then Format.eprintf "Specialize js once...@.";
  Specialize_js.f_once p

let specialize' ~target (p, info) =
  let p = specialize_1 (p, info) in
  let p = specialize_js ~target (p, info) in
  p, info

let specialize ~target p = fst (specialize' ~target p)

let eval ~target (p, info) =
  if Config.Flag.staticeval () then Eval.f ~target info p else p

let flow p =
  if debug () then Format.eprintf "Data flow...@.";
  Flow.f p

let flow_simple p =
  if debug () then Format.eprintf "Data flow...@.";
  Flow.f ~skip_param:true p

let phi p =
  if debug () then Format.eprintf "Variable passing simplification...@.";
  Phisimpl.f p

let ( +> ) f g x = g (f x)

let map_fst f (x, y) = f x, y

let effects p =
  if Config.Flag.effects ()
  then (
    if debug () then Format.eprintf "Effects...@.";
    p |> Deadcode.f +> Effects.f +> map_fst Lambda_lifting.f)
  else p, (Code.Var.Set.empty : Effects.cps_calls)

let exact_calls profile p =
  if not (Config.Flag.effects ())
  then
    let fast =
      match profile with
      | O3 -> false
      | O1 | O2 -> true
    in
    let info = Global_flow.f ~fast p in
    Specialize.f ~function_arity:(fun f -> Global_flow.function_arity info f) p
  else p

let print p =
  if debug () then Code.Print.program (fun _ _ -> "") p;
  p

let rec loop max name round i (p : 'a) : 'a =
  let p' = round p in
  if i >= max || Code.eq p' p
  then p'
  else (
    if times () then Format.eprintf "Start Iteration (%s) %d...@." name i;
    loop max name round (i + 1) p')

let identity x = x

(* o1 *)

let o1 ~target : 'a -> 'a =
  print
  +> tailcall
  +> flow_simple (* flow simple to keep information for future tailcall opt *)
  +> specialize' ~target
  +> eval ~target
  +> inline ~target (* inlining may reveal new tailcall opt *)
  +> deadcode
  +> tailcall
  +> phi
  +> flow
  +> specialize' ~target
  +> eval ~target
  +> inline ~target
  +> deadcode
  +> print
  +> flow
  +> specialize' ~target
  +> eval ~target
  +> inline ~target
  +> deadcode
  +> phi
  +> flow
  +> specialize ~target
  +> identity

(* o2 *)

let o2 ~target : 'a -> 'a = loop 10 "o1" (o1 ~target) 1 +> print

(* o3 *)

let round1 ~target : 'a -> 'a =
  print
  +> tailcall
  +> inline ~target (* inlining may reveal new tailcall opt *)
  +> deadcode (* deadcode required before flow simple -> provided by constant *)
  +> flow_simple (* flow simple to keep information for future tailcall opt *)
  +> specialize' ~target
  +> eval ~target
  +> identity

let round2 ~target = flow +> specialize' ~target +> eval ~target +> deadcode +> o1 ~target

let o3 ~target =
  loop 10 "tailcall+inline" (round1 ~target) 1
  +> loop 10 "flow" (round2 ~target) 1
  +> print

let generate
    d
    ~exported_runtime
    ~wrap_with_fun
    ~warn_on_unhandled_effect
    ((p, live_vars), cps_calls) =
  if times () then Format.eprintf "Start Generation...@.";
  let should_export = should_export wrap_with_fun in
  Generate.f
    p
    ~exported_runtime
    ~live_vars
    ~cps_calls
    ~should_export
    ~warn_on_unhandled_effect
    d

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
                  , dot (EVar (ident Constant.global_object_)) prim
                  , EVar (ident_s "undefined") )
              , dot (EVar (ident Constant.global_object_)) prim
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

let link ~standalone ~linkall (js : Javascript.statement_list) : Linker.output =
  if not standalone
  then { runtime_code = js; always_required_codes = [] }
  else
    let t = Timer.make () in
    if times () then Format.eprintf "Start Linking...@.";
    let traverse = new Js_traverse.free in
    let js = traverse#program js in
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
    let free = traverse#get_free in
    let free : StringSet.t =
      Javascript.IdentSet.fold
        (fun x acc ->
          match x with
          | V _ ->
              (* This is an error. We don't complain here as we want
                 to be able to name other variable to make it
                 easier to spot the problematic ones *)
              acc
          | S { name = Utf8 x; _ } -> StringSet.add x acc)
        free
        StringSet.empty
    in
    let prim = Primitive.get_external () in
    let prov = Linker.get_provided () in
    let all_external = StringSet.union prim prov in
    let used = StringSet.inter free all_external in
    let linkinfos = Linker.init () in
    let linkinfos, missing = Linker.resolve_deps ~linkall linkinfos used in
    (* gen_missing may use caml_failwith *)
    let linkinfos, missing =
      if (not (StringSet.is_empty missing)) && Config.Flag.genprim ()
      then
        let linkinfos, missing2 =
          Linker.resolve_deps linkinfos (StringSet.singleton "caml_failwith")
        in
        linkinfos, StringSet.union missing missing2
      else linkinfos, missing
    in
    let js = if Config.Flag.genprim () then gen_missing js missing else js in
    if times () then Format.eprintf "  linking: %a@." Timer.print t;
    let js =
      if linkall
      then
        let open Javascript in
        let all = Linker.all linkinfos in
        let all =
          List.map all ~f:(fun name ->
              let name = Utf8_string.of_string_exn name in
              Property (PNI name, EVar (ident name)))
        in
        ( Expression_statement
            (EBin
               ( Eq
               , dot
                   (EVar (ident Constant.global_object_))
                   (Utf8_string.of_string_exn "jsoo_runtime")
               , EObj all ))
        , N )
        :: js
      else js
    in
    Linker.link js linkinfos

let check_js js =
  let t = Timer.make () in
  if times () then Format.eprintf "Start Checks...@.";
  let traverse = new Js_traverse.free in
  let js = traverse#program js in
  let free = traverse#get_free in
  let free : StringSet.t =
    Javascript.IdentSet.fold
      (fun x acc ->
        match x with
        | V _ -> assert false
        | S { name = Utf8 x; _ } -> StringSet.add x acc)
      free
      StringSet.empty
  in
  let prim = Primitive.get_external () in
  let prov = Linker.get_provided () in
  let all_external = StringSet.union prim prov in
  let missing = StringSet.inter free all_external in
  let missing = StringSet.diff missing Reserved.provided in
  let other = StringSet.diff free missing in
  let res = Var_printer.get_reserved () in
  let other = StringSet.diff other res in
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

let coloring js =
  let t = Timer.make () in
  if times () then Format.eprintf "Start Coloring...@.";
  let traverse = new Js_traverse.free in
  let js = traverse#program js in
  let free = traverse#get_free in
  Javascript.IdentSet.iter
    (fun x ->
      match x with
      | V _ ->
          (* This is an error. We don't complain here as we want
             to be able to name other variable to make it
             easier to spot the problematic ones *)
          ()
      | S { name = Utf8 x; _ } -> Var_printer.add_reserved x)
    free;
  let js = Js_assign.program js in
  if times () then Format.eprintf "  coloring: %a@." Timer.print t;
  js

let output formatter ~source_map () js =
  let t = Timer.make () in
  if times () then Format.eprintf "Start Writing file...@.";
  let sm = Js_output.program formatter ?source_map js in
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
      let js = (new Js_traverse.share_constant)#program js in
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
    let freenames =
      let o = new Js_traverse.free in
      let (_ : J.program) = o#program js in
      o#get_free
    in
    let export_shim js =
      if J.IdentSet.mem (J.ident Constant.exports_) freenames
      then
        if should_export wrap_with_fun
        then var Constant.exports_ (J.EObj []) :: js
        else
          let export_node =
            let s =
              Printf.sprintf
                {|((typeof module === 'object' && module.exports) || %s)|}
                Constant.global_object
            in
            let lex = Parse_js.Lexer.of_string s in
            Parse_js.parse_expr lex
          in
          var Constant.exports_ export_node :: js
      else js
    in
    let old_global_object_shim js =
      if J.IdentSet.mem (J.ident Constant.old_global_object_) freenames
      then
        var Constant.old_global_object_ (J.EVar (J.ident Constant.global_object_)) :: js
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
      f [ J.ident Constant.global_object_ ] js
    in
    match wrap_with_fun with
    | `Anonymous -> expr (mk efun)
    | `Named name ->
        let name = Utf8_string.of_string_exn name in
        mk (sfun (J.ident name))
    | `Iife -> expr (J.call (mk efun) [ J.EVar (J.ident Constant.global_object_) ] J.N)
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
  (* post pack optim *)
  let t3 = Timer.make () in
  let js = (new Js_traverse.simpl)#program js in
  if times () then Format.eprintf "    simpl: %a@." Timer.print t3;
  let t4 = Timer.make () in
  let js = (new Js_traverse.clean)#program js in
  if times () then Format.eprintf "    clean: %a@." Timer.print t4;
  let js =
    if Config.Flag.shortvar ()
    then (
      let t5 = Timer.make () in
      let js = (new Js_traverse.rename_variable)#program js in
      if times () then Format.eprintf "    shortten vars: %a@." Timer.print t5;
      js)
    else js
  in
  if times () then Format.eprintf "  optimizing: %a@." Timer.print t;
  js

let configure formatter =
  let pretty = Config.Flag.pretty () in
  Pretty_print.set_compact formatter (not pretty);
  Code.Var.set_pretty (pretty && not (Config.Flag.shortvar ()));
  Code.Var.set_stable (Config.Flag.stable_var ())

let target_flag t =
  match t with
  | `JavaScript _ -> `JavaScript
  | `Wasm _ -> `Wasm

let full ~target ~standalone ~wrap_with_fun ~profile ~linkall ~source_map d p =
  let exported_runtime = not standalone in
  let opt =
    specialize_js_once
    +> (match profile with
       | O1 -> o1
       | O2 -> o2
       | O3 -> o3)
         ~target:(target_flag target)
    +> exact_calls profile
    +> effects
    +> map_fst
         ((match target with
          | `JavaScript _ -> Generate_closure.f
          | `Wasm _ -> Fun.id)
         +> deadcode')
  in
  let emit formatter =
    generate d ~exported_runtime ~wrap_with_fun ~warn_on_unhandled_effect:standalone
    +> link ~standalone ~linkall
    +> pack ~wrap_with_fun ~standalone
    +> coloring
    +> check_js
    +> output formatter ~source_map ()
  in
  if times () then Format.eprintf "Start Optimizing...@.";
  let t = Timer.make () in
  let r = opt p in
  let () = if times () then Format.eprintf " optimizations : %a@." Timer.print t in
  match target with
  | `JavaScript formatter -> emit formatter r
  | `Wasm ch ->
      let (p, live_vars), _ = r in
      Wa_generate.f ch ~live_vars p;
      None

let full_no_source_map ~target ~standalone ~wrap_with_fun ~profile ~linkall d p =
  let (_ : Source_map.t option) =
    full ~target ~standalone ~wrap_with_fun ~profile ~linkall ~source_map:None d p
  in
  ()

let f
    ~target
    ?(standalone = true)
    ?(wrap_with_fun = `Iife)
    ?(profile = O1)
    ?(linkall = false)
    ?source_map
    d
    p =
  full ~target ~standalone ~wrap_with_fun ~profile ~linkall ~source_map d p

let f'
    ?(standalone = true)
    ?(wrap_with_fun = `Iife)
    ?(profile = O1)
    ?(linkall = false)
    formatter
    d
    p =
  full_no_source_map
    ~target:(`JavaScript formatter)
    ~standalone
    ~wrap_with_fun
    ~profile
    ~linkall
    d
    p

let from_string ~prims ~debug s formatter =
  let p, d = Parse_bytecode.from_string ~prims ~debug s in
  full_no_source_map
    ~target:(`JavaScript formatter)
    ~standalone:false
    ~wrap_with_fun:`Anonymous
    ~profile:O1
    ~linkall:false
    d
    p

let profiles = [ 1, O1; 2, O2; 3, O3 ]

let profile i = try Some (List.assoc i profiles) with Not_found -> None
