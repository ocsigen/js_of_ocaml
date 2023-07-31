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

type 'a pack =
  | Ok of 'a
  | Pack of string

let unpack = function
  | Ok x -> x
  | Pack x -> Marshal.from_string x 0

let pack = function
  | Pack _ as x -> x
  | Ok x -> Pack (Marshal.to_string x [])

let to_stringset utf8_string_set =
  Javascript.IdentSet.fold
    (fun x acc ->
      match x with
      | S { name = Utf8 x; _ } -> StringSet.add x acc
      | V _ -> acc)
    utf8_string_set
    StringSet.empty

let loc pi =
  match pi with
  | { Parse_info.src = Some src; line; _ } | { Parse_info.name = Some src; line; _ } ->
      Printf.sprintf "%s:%d" src line
  | _ -> "unknown location"

let error s = Format.ksprintf (fun s -> failwith s) s

module Arity : sig
  val find : Javascript.program -> name:string -> int option
end = struct
  let rec find p ~name =
    match p with
    | [] -> None
    | ( Javascript.Function_declaration
          (Javascript.S { Javascript.name = Utf8 n; _ }, (_, { list; rest = None }, _, _))
      , _ )
      :: _
      when String.equal name n -> Some (List.length list)
    | _ :: rem -> find rem ~name
end

module Named_value : sig
  val find_all : Javascript.program -> StringSet.t
end = struct
  class traverse_and_find_named_values all =
    object
      inherit Js_traverse.iter as self

      method expression x =
        let open Javascript in
        (match x with
        | ECall
            (EVar (S { name = Utf8 "caml_named_value"; _ }), _, [ Arg (EStr (Utf8 v)) ], _)
          -> all := StringSet.add v !all
        | _ -> ());
        self#expression x
    end

  let find_all code =
    let all = ref StringSet.empty in
    let p = new traverse_and_find_named_values all in
    p#program code;
    !all
end

module Check = struct
  class check_and_warn name pi =
    object
      inherit Js_traverse.free as super

      method merge_info from =
        let def = from#get_def in
        let use = from#get_use in
        let diff = Javascript.IdentSet.diff def use in
        let diff =
          Javascript.IdentSet.fold
            (fun x acc ->
              match x with
              | S { name = Utf8_string.Utf8 s; _ } ->
                  if String.is_prefix s ~prefix:"_" || String.equal s name
                  then acc
                  else s :: acc
              | V _ -> acc)
            diff
            []
        in

        (match diff with
        | [] -> ()
        | l ->
            warn
              "WARN unused for primitive %s at %s:@. %s@."
              name
              (loc pi)
              (String.concat ~sep:", " l));
        super#merge_info from
    end

  let primitive ~name pi ~code ~requires ~has_flags =
    let free =
      if Config.Flag.warn_unused ()
      then new check_and_warn name pi
      else new Js_traverse.free
    in
    let _code = free#program code in
    let freename = to_stringset free#get_free in
    let freename =
      List.fold_left requires ~init:freename ~f:(fun freename x ->
          StringSet.remove x freename)
    in
    let freename = StringSet.diff freename Reserved.keyword in
    let freename = StringSet.diff freename Reserved.provided in
    let freename = StringSet.remove Constant.global_object freename in
    let freename = if has_flags then StringSet.remove "FLAG" freename else freename in
    if StringSet.mem Constant.old_global_object freename && false
       (* Don't warn yet, we want to give a transition period where both
          "globalThis" and "joo_global_object" are allowed without extra
          noise *)
    then
      warn
        "warning: %s: 'joo_global_object' is being deprecated, please use `globalThis` \
         instead@."
        (loc pi);
    let freename = StringSet.remove Constant.old_global_object freename in
    let defname = to_stringset free#get_def in
    if not (StringSet.mem name defname)
    then
      warn
        "warning: primitive code does not define value with the expected name: %s (%s)@."
        name
        (loc pi);
    if not (StringSet.is_empty freename)
    then (
      warn "warning: free variables in primitive code %S (%s)@." name (loc pi);
      warn "vars: %s@." (String.concat ~sep:", " (StringSet.elements freename)))
end

module Fragment = struct
  type provides =
    { parse_info : Parse_info.t
    ; name : string
    ; kind : Primitive.kind
    ; kind_arg : Primitive.kind_arg list option
    ; arity : int option
    ; named_values : StringSet.t
    }

  type fragment_ =
    { provides : provides option
    ; requires : string list
    ; has_macro : bool
    ; version_constraint_ok : bool
    ; weakdef : bool
    ; always : bool
    ; code : Javascript.program pack
    ; js_string : bool option
    ; effects : bool option
    ; fragment_target : Target_env.t option
    ; aliases : StringSet.t
    }

  type t =
    | Always_include of Javascript.program pack
    | Fragment of fragment_

  let provides = function
    | Always_include _ -> []
    | Fragment { provides = Some p; _ } -> [ p.name ]
    | Fragment _ -> []

  let analyze (t : t) : t =
    match t with
    | Always_include _ -> t
    | Fragment { provides = None; _ } -> t
    | Fragment
        ({ provides =
             Some
               ({ parse_info = pi
                ; name
                ; kind = _
                ; kind_arg = _
                ; arity = _
                ; named_values = _
                } as provides)
         ; _
         } as fragment) ->
        let code, has_flags = Macro.f ~flags:false (unpack fragment.code) in
        let named_values = Named_value.find_all code in
        let arity = Arity.find code ~name in
        Check.primitive ~has_flags ~name pi ~code ~requires:fragment.requires;
        let provides = Some { provides with named_values; arity } in
        Fragment { fragment with code = Ok code; provides; has_macro = has_flags }

  let version_match =
    List.for_all ~f:(fun (op, str) -> op Ocaml_version.(compare current (split str)) 0)

  let parse_from_lex ~filename lex =
    let program, _ =
      try Parse_js.parse' lex
      with Parse_js.Parsing_error pi ->
        let name =
          match pi with
          | { Parse_info.src = Some x; _ } | { Parse_info.name = Some x; _ } -> x
          | _ -> "??"
        in
        error
          "cannot parse file %S (orig:%S from l:%d, c:%d)@."
          filename
          name
          pi.Parse_info.line
          pi.Parse_info.col
    in
    let res =
      List.map program ~f:(fun (annot, code) ->
          match annot with
          | [] -> Always_include (Ok code)
          | annot ->
              let initial_fragment : fragment_ =
                { provides = None
                ; requires = []
                ; version_constraint_ok = true
                ; weakdef = false
                ; always = false
                ; has_macro = false
                ; code = Ok code
                ; js_string = None
                ; effects = None
                ; fragment_target = None
                ; aliases = StringSet.empty
                }
              in
              let fragment =
                List.fold_left
                  annot
                  ~init:initial_fragment
                  ~f:(fun (fragment : fragment_) ((_, a), pi) ->
                    match a with
                    | `Provides (name, kind, ka) ->
                        { fragment with
                          provides =
                            Some
                              { parse_info = pi
                              ; name
                              ; kind
                              ; kind_arg = ka
                              ; arity = None
                              ; named_values = StringSet.empty
                              }
                        }
                    | `Requires mn -> { fragment with requires = mn @ fragment.requires }
                    | `Version l ->
                        { fragment with
                          version_constraint_ok =
                            fragment.version_constraint_ok && version_match l
                        }
                    | `Weakdef -> { fragment with weakdef = true }
                    | `Always -> { fragment with always = true }
                    | `Alias name ->
                        { fragment with aliases = StringSet.add name fragment.aliases }
                    | (`Ifnot "js-string" | `If "js-string") as i ->
                        let b =
                          match i with
                          | `If _ -> true
                          | `Ifnot _ -> false
                        in
                        if Option.is_some fragment.js_string
                        then Format.eprintf "Duplicated js-string in %s\n" (loc pi);
                        { fragment with js_string = Some b }
                    | (`Ifnot "effects" | `If "effects") as i ->
                        let b =
                          match i with
                          | `If _ -> true
                          | `Ifnot _ -> false
                        in
                        if Option.is_some fragment.effects
                        then Format.eprintf "Duplicated effects in %s\n" (loc pi);
                        { fragment with effects = Some b }
                    | `If name when Option.is_some (Target_env.of_string name) ->
                        if Option.is_some fragment.fragment_target
                        then Format.eprintf "Duplicated target_env in %s\n" (loc pi);
                        { fragment with fragment_target = Target_env.of_string name }
                    | `If name | `Ifnot name ->
                        Format.eprintf "Unkown flag %S in %s\n" name (loc pi);
                        fragment)
              in
              Fragment fragment)
    in
    List.map ~f:analyze res

  let parse_builtin builtin =
    let filename = Builtins.File.name builtin in
    let content = Builtins.File.content builtin in
    match Builtins.File.fragments builtin with
    | None ->
        let lex = Parse_js.Lexer.of_string ~filename content in
        parse_from_lex ~filename lex
    | Some fragments -> Marshal.from_string fragments 0

  let parse_string string =
    let filename = "<string>" in
    let lex = Parse_js.Lexer.of_string ~filename string in
    parse_from_lex ~filename lex

  let parse_file f =
    let file =
      match Findlib.find [] f with
      | Some file -> Fs.absolute_path file
      | None -> error "cannot find file '%s'. @." f
    in
    let lex = Parse_js.Lexer.of_file file in
    parse_from_lex ~filename:file lex

  let pack = function
    | Always_include x -> Always_include (pack x)
    | Fragment f -> Fragment { f with code = pack f.code }
end

(*
exception May_not_return

let all_return p =
  let open Javascript in
  let rec loop_st = function
    | [] -> raise  May_not_return
    | [Return_statement (Some _), _] -> ()
    | [Return_statement None, _] -> raise May_not_return
    | [If_statement(_,th,el), _] ->
      loop_st [th];
      (match el with
       | None -> raise May_not_return
       | Some x -> loop_st [x])
    | [Do_while_statement(st,_), _] -> loop_st [st]
    | [While_statement(_,st), _] -> loop_st [st]
    | [For_statement (_,_,_,st), _] -> loop_st [st]
    | [Switch_statement (_,l,def), _] ->
      List.iter (fun (_,sts) -> loop_st sts) l
    | [Try_statement(b,_,_),_] -> loop_st b
    | [Throw_statement _, _] -> ()
    | x::xs -> loop_st xs
  in
  let rec loop_sources = function
    | [] -> raise May_not_return
    | [(Statement x, loc)] -> loop_st [(x, loc)]
    | [_] -> raise May_not_return
    | x::xs -> loop_sources xs
  in
  let rec loop_all_sources = function
    | [] -> ()
    | Statement x :: xs -> loop_all_sources xs
    | Function_declaration(_,_,b,_) :: xs ->
      loop_sources b;
      loop_all_sources xs in
  try loop_all_sources p; true with May_not_return -> false
*)

type always_required' =
  { ar_filename : string
  ; ar_program : Javascript.program pack
  ; ar_requires : string list
  }

type always_required =
  { filename : string
  ; program : Javascript.program
  ; requires : string list
  }

type state =
  { ids : IntSet.t
  ; always_required_codes : always_required list
  ; codes : (Javascript.program pack * bool) list
  }

type output =
  { runtime_code : Javascript.program
  ; always_required_codes : always_required list
  }

type provided =
  { id : int
  ; pi : Parse_info.t
  ; weakdef : bool
  ; target_env : Target_env.t
  }

let always_included = ref []

let provided = Hashtbl.create 31

let provided_rev = Hashtbl.create 31

let code_pieces = Hashtbl.create 31

let reset () =
  always_included := [];
  Hashtbl.clear provided;
  Hashtbl.clear provided_rev;
  Hashtbl.clear code_pieces;
  Primitive.reset ();
  Generate.init ()

let load_fragment ~ignore_always_annotation ~target_env ~filename (f : Fragment.t) =
  match f with
  | Always_include code ->
      always_included :=
        { ar_filename = filename; ar_program = code; ar_requires = [] }
        :: !always_included;
      `Ok
  | Fragment
      { provides
      ; requires
      ; version_constraint_ok
      ; weakdef
      ; always
      ; code
      ; js_string
      ; effects
      ; fragment_target
      ; aliases
      ; has_macro
      } -> (
      let ignore_because_of_js_string =
        match js_string, Config.Flag.use_js_string () with
        | Some true, false | Some false, true -> true
        | None, _ | Some true, true | Some false, false -> false
      in
      let ignore_because_of_effects =
        match effects, Config.Flag.effects () with
        | Some true, false | Some false, true -> true
        | None, _ | Some true, true | Some false, false -> false
      in
      if (not version_constraint_ok)
         || ignore_because_of_js_string
         || ignore_because_of_effects
      then `Ignored
      else
        match provides with
        | None ->
            if not (StringSet.is_empty aliases)
            then
              error
                "Found JavaScript code with neither `//Alias` and not `//Provides` in \
                 file %S@."
                filename;
            if always
            then (
              if not ignore_always_annotation
              then
                always_included :=
                  { ar_filename = filename; ar_program = code; ar_requires = requires }
                  :: !always_included;
              `Ok)
            else
              error
                "Found JavaScript code with neither `//Provides` nor `//Always` in file \
                 %S@."
                filename
        | Some { parse_info = pi; name; kind; kind_arg = ka; arity; named_values } ->
            let fragment_target =
              Option.value ~default:Target_env.Isomorphic fragment_target
            in
            let exists =
              try `Exists (Hashtbl.find provided name) with Not_found -> `New
            in
            let is_updating =
              match
                exists, (target_env : Target_env.t), (fragment_target : Target_env.t)
              with
              (* permit default, un-annotated symbols *)
              | `New, _, Isomorphic -> true
              (* permit env specializations *)
              | `New, Nodejs, Nodejs
              | `New, Browser, Browser
              | `Exists { target_env = Isomorphic; _ }, Nodejs, Nodejs
              | `Exists { target_env = Isomorphic; _ }, Browser, Browser -> true
              (* ignore non target matched envs *)
              | (`Exists _ | `New), Isomorphic, (Browser | Nodejs)
              | (`Exists _ | `New), Browser, Nodejs
              | (`Exists _ | `New), Nodejs, Browser -> false
              (* Ignore env unspecializations *)
              | `Exists { target_env = Nodejs; _ }, Nodejs, Isomorphic
              | `Exists { target_env = Browser; _ }, Browser, Isomorphic -> false
              (* The following are impossible *)
              | `Exists { target_env = Nodejs; _ }, Browser, _
              | `Exists { target_env = Browser; _ }, Nodejs, _
              | `Exists { target_env = Nodejs | Browser; _ }, Isomorphic, _ ->
                  assert false
              (* collision detected *)
              | `Exists ({ target_env = Nodejs; _ } as p), Nodejs, Nodejs
              | `Exists ({ target_env = Isomorphic; _ } as p), Nodejs, Isomorphic
              | `Exists ({ target_env = Browser; _ } as p), Browser, Browser
              | `Exists ({ target_env = Isomorphic; _ } as p), Browser, Isomorphic
              | `Exists ({ target_env = Isomorphic; _ } as p), Isomorphic, Isomorphic ->
                  if p.weakdef
                  then true
                  else (
                    warn
                      "warning: overriding primitive %S\n  old: %s\n  new: %s@."
                      name
                      (loc p.pi)
                      (loc pi);
                    true)
            in
            if not is_updating
            then `Ignored
            else
              let () = () in
              let id = Hashtbl.length provided in
              Primitive.register name kind ka arity;
              StringSet.iter Primitive.register_named_value named_values;
              Hashtbl.add provided name { id; pi; weakdef; target_env = fragment_target };
              Hashtbl.add provided_rev id (name, pi);
              Hashtbl.add code_pieces id (code, has_macro, requires);
              StringSet.iter (fun alias -> Primitive.alias alias name) aliases;
              `Ok)

let get_provided () =
  Hashtbl.fold (fun k _ acc -> StringSet.add k acc) provided StringSet.empty

let check_deps () =
  let provided = get_provided () in
  Hashtbl.iter
    (fun id (code, _has_macro, requires) ->
      match code with
      | Ok code -> (
          let traverse = new Js_traverse.free in
          let _js = traverse#program code in
          let free = to_stringset traverse#get_free in
          let requires =
            List.fold_right requires ~init:StringSet.empty ~f:StringSet.add
          in
          let real = StringSet.inter free provided in
          let missing = StringSet.diff real requires in
          if not (StringSet.is_empty missing)
          then
            try
              let name, ploc = Hashtbl.find provided_rev id in
              warn
                "code providing %s (%s) may miss dependencies: %s\n"
                name
                (loc ploc)
                (String.concat ~sep:", " (StringSet.elements missing))
            with Not_found ->
              (* there is no //Provides for this piece of code *)
              (* FIXME handle missing deps in this case *)
              ())
      | Pack _ ->
          (* We only have [Pack] for the builtin runtime, which has
             been checked already (before it was embedded *)
          ())
    code_pieces

let load_file ~ignore_always_annotation ~target_env filename =
  List.iter (Fragment.parse_file filename) ~f:(fun frag ->
      let (`Ok | `Ignored) =
        load_fragment ~ignore_always_annotation ~target_env ~filename frag
      in
      ())

let load_fragments ?(ignore_always_annotation = false) ~target_env ~filename l =
  List.iter l ~f:(fun frag ->
      let (`Ok | `Ignored) =
        load_fragment ~ignore_always_annotation ~target_env ~filename frag
      in
      ());
  check_deps ()

let load_files ?(ignore_always_annotation = false) ~target_env l =
  List.iter l ~f:(fun filename ->
      load_file ~ignore_always_annotation ~target_env filename);
  check_deps ()

(* resolve *)
let rec resolve_dep_name_rev visited path nm =
  let x =
    try Hashtbl.find provided nm with Not_found -> error "missing dependency '%s'@." nm
  in
  resolve_dep_id_rev visited path x.id

and resolve_dep_id_rev visited path id =
  if IntSet.mem id visited.ids
  then (
    if List.memq id ~set:path
    then
      error
        "circular dependency: %s"
        (String.concat
           ~sep:", "
           (List.map path ~f:(fun id -> fst (Hashtbl.find provided_rev id))));
    visited)
  else
    let path = id :: path in
    let code, has_macro, req = Hashtbl.find code_pieces id in
    let visited = { visited with ids = IntSet.add id visited.ids } in
    let visited =
      List.fold_left req ~init:visited ~f:(fun visited nm ->
          resolve_dep_name_rev visited path nm)
    in
    let visited = { visited with codes = (code, has_macro) :: visited.codes } in
    visited

let proj_always_required { ar_filename; ar_requires; ar_program } =
  { filename = ar_filename; requires = ar_requires; program = unpack ar_program }

let init () =
  { ids = IntSet.empty
  ; always_required_codes = List.rev_map !always_included ~f:proj_always_required
  ; codes = []
  }

let resolve_deps ?(linkall = false) visited_rev used =
  (* link the special files *)
  let missing, visited_rev =
    if linkall
    then
      (* link all primitives *)
      let prog, set =
        Hashtbl.fold
          (fun nm _ (visited, set) ->
            resolve_dep_name_rev visited [] nm, StringSet.add nm set)
          provided
          (visited_rev, StringSet.empty)
      in
      let missing = StringSet.diff used set in
      missing, prog
    else
      (* link used primitives *)
      StringSet.fold
        (fun nm (missing, visited) ->
          if Hashtbl.mem provided nm
          then missing, resolve_dep_name_rev visited [] nm
          else StringSet.add nm missing, visited)
        used
        (StringSet.empty, visited_rev)
  in
  visited_rev, missing

let link program (state : state) =
  let always, always_required =
    List.partition
      ~f:(function
        | { requires = []; _ } -> false
        | _ -> true)
      state.always_required_codes
  in

  let state =
    List.fold_left always ~init:state ~f:(fun (state : state) always ->
        let state =
          List.fold_left always.requires ~init:state ~f:(fun state nm ->
              resolve_dep_name_rev state [] nm)
        in
        { state with codes = (Ok always.program, false) :: state.codes })
  in
  let codes =
    List.map state.codes ~f:(fun (x, has_macro) ->
        let c = unpack x in
        if has_macro
        then
          let c, _ = Macro.f ~flags:true c in
          c
        else c)
  in
  let runtime = List.flatten (List.rev (program :: codes)) in
  { runtime_code = runtime; always_required_codes = always_required }

let all state =
  IntSet.fold
    (fun id acc ->
      try
        let name, _ = Hashtbl.find provided_rev id in
        name :: acc
      with Not_found -> acc)
    state.ids
    []

let origin ~name =
  try
    let x = Hashtbl.find provided name in
    x.pi.Parse_info.src
  with Not_found -> None
