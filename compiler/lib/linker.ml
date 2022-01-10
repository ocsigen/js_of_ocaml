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

module Fragment = struct
  type fragment_ =
    { provides :
        (Parse_info.t * string * Primitive.kind * Primitive.kind_arg list option) option
    ; requires : string list
    ; version_constraint : ((int -> int -> bool) * string) list list
    ; weakdef : bool
    ; always : bool
    ; code : Javascript.program
    ; js_string : bool option
    ; fragment_target : Target_env.t option
    }

  type t =
    [ `Always_include of Javascript.program
    | `Some of fragment_
    ]

  let provides = function
    | `Always_include _ -> []
    | `Some { provides = Some (_, name, _, _); _ } -> [ name ]
    | `Some _ -> []
end

let loc pi =
  match pi with
  | { Parse_info.src = Some src; line; _ } | { Parse_info.name = Some src; line; _ } ->
      Printf.sprintf "%s:%d" src line
  | _ -> "unknown location"

let error s = Format.ksprintf (fun s -> failwith s) s

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
  let rec collect_without_annot acc = function
    | [] -> List.rev acc, []
    | (x, []) :: program -> collect_without_annot (x :: acc) program
    | (_, _ :: _) :: _ as program -> List.rev acc, program
  in
  let rec collect acc program =
    match program with
    | [] -> List.rev acc
    | (x, []) :: program ->
        let code, program = collect_without_annot [ x ] program in
        collect (([], code) :: acc) program
    | (x, annots) :: program ->
        let code, program = collect_without_annot [ x ] program in
        collect ((annots, code) :: acc) program
  in
  let blocks = collect [] program in
  let res =
    List.rev_map blocks ~f:(fun (annot, code) ->
        match annot with
        | [] -> `Always_include code
        | annot ->
            let initial_fragment : Fragment.fragment_ =
              { provides = None
              ; requires = []
              ; version_constraint = []
              ; weakdef = false
              ; always = false
              ; code
              ; js_string = None
              ; fragment_target = None
              }
            in
            let fragment =
              List.fold_left
                annot
                ~init:initial_fragment
                ~f:(fun (fragment : Fragment.fragment_) (_, pi, a) ->
                  match a with
                  | `Provides (name, kind, ka) ->
                      { fragment with provides = Some (pi, name, kind, ka) }
                  | `Requires mn -> { fragment with requires = mn @ fragment.requires }
                  | `Version l ->
                      { fragment with
                        version_constraint = l :: fragment.version_constraint
                      }
                  | `Weakdef -> { fragment with weakdef = true }
                  | `Always -> { fragment with always = true }
                  | (`Ifnot "js-string" | `If "js-string") as i ->
                      let b =
                        match i with
                        | `If _ -> true
                        | `Ifnot _ -> false
                      in
                      if Option.is_some fragment.js_string
                      then Format.eprintf "Duplicated js-string in %s\n" (loc pi);
                      { fragment with js_string = Some b }
                  | `If name when Option.is_some (Target_env.of_string name) ->
                      if Option.is_some fragment.fragment_target
                      then Format.eprintf "Duplicated target_env in %s\n" (loc pi);
                      { fragment with fragment_target = Target_env.of_string name }
                  | `If name | `Ifnot name ->
                      Format.eprintf "Unkown flag %S in %s\n" name (loc pi);
                      fragment)
            in
            `Some fragment)
  in
  res

let parse_builtin builtin =
  let filename = Builtins.File.name builtin in
  let content = Builtins.File.content builtin in
  let lexbuf = Lexing.from_string content in
  let lexbuf =
    { lexbuf with lex_curr_p = { lexbuf.lex_curr_p with pos_fname = filename } }
  in
  let lex = Parse_js.Lexer.of_lexbuf lexbuf in
  parse_from_lex ~filename lex

let parse_string string =
  let lexbuf = Lexing.from_string string in
  let lex = Parse_js.Lexer.of_lexbuf lexbuf in
  parse_from_lex ~filename:"<dummy>" lex

let parse_file f =
  let file =
    try
      match Findlib.path_require_findlib f with
      | Some f ->
          let pkg, f' =
            match String.split ~sep:Filename.dir_sep f with
            | [] -> assert false
            | pkg :: l -> pkg, List.fold_left l ~init:"" ~f:Filename.concat
          in
          Fs.absolute_path (Filename.concat (Findlib.find_pkg_dir pkg) f')
      | None -> Fs.absolute_path f
    with
    | Not_found -> error "cannot find file '%s'. @." f
    | Sys_error s -> error "%s@." s
  in
  let lex = Parse_js.Lexer.of_file file in
  parse_from_lex ~filename:file lex

class check_and_warn name pi =
  object
    inherit Js_traverse.free as super

    method merge_info from =
      let def = from#get_def_name in
      let use = from#get_use_name in
      let diff = StringSet.diff def use in
      let diff = StringSet.remove name diff in
      let diff = StringSet.filter (fun s -> not (String.is_prefix s ~prefix:"_")) diff in
      if not (StringSet.is_empty diff)
      then
        warn
          "WARN unused for primitive %s at %s:@. %s@."
          name
          (loc pi)
          (String.concat ~sep:", " (StringSet.elements diff));
      super#merge_info from
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

let check_primitive ~name pi ~code ~requires =
  let free =
    if Config.Flag.warn_unused ()
    then new check_and_warn name pi
    else new Js_traverse.free
  in
  let _code = free#program code in
  let freename = free#get_free_name in
  let freename =
    List.fold_left requires ~init:freename ~f:(fun freename x ->
        StringSet.remove x freename)
  in
  let freename = StringSet.diff freename Reserved.keyword in
  let freename = StringSet.diff freename Reserved.provided in
  let freename = StringSet.remove Constant.global_object freename in
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
  if not (StringSet.mem name free#get_def_name)
  then
    warn
      "warning: primitive code does not define value with the expected name: %s (%s)@."
      name
      (loc pi);
  if not (StringSet.is_empty freename)
  then (
    warn "warning: free variables in primitive code %S (%s)@." name (loc pi);
    warn "vars: %s@." (String.concat ~sep:", " (StringSet.elements freename)))

let version_match =
  List.for_all ~f:(fun (op, str) -> op Ocaml_version.(compare current (split str)) 0)

type always_required =
  { filename : string
  ; program : Javascript.program
  ; requires : string list
  }

type state =
  { ids : IntSet.t
  ; always_required_codes : always_required list
  ; codes : Javascript.program list
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

let last_code_id = ref 0

let always_included = ref []

let provided = Hashtbl.create 31

let provided_rev = Hashtbl.create 31

let code_pieces = Hashtbl.create 31

let reset () =
  last_code_id := 0;
  always_included := [];
  Hashtbl.clear provided;
  Hashtbl.clear provided_rev;
  Hashtbl.clear code_pieces

class traverse_and_find_named_values all =
  object
    inherit Js_traverse.map as self

    method expression x =
      let open Javascript in
      (match x with
      | ECall
          (EVar (S { name = "caml_named_value"; _ }), [ (EStr (v, _), `Not_spread) ], _)
        -> all := StringSet.add v !all
      | _ -> ());
      self#expression x
  end

let find_named_value code =
  let all = ref StringSet.empty in
  let p = new traverse_and_find_named_values all in
  ignore (p#program code);
  !all

let load_fragment ~target_env ~filename (f : Fragment.t) =
  match f with
  | `Always_include code ->
      always_included := { filename; program = code; requires = [] } :: !always_included;
      `Ok
  | `Some
      { provides
      ; requires
      ; version_constraint
      ; weakdef
      ; always
      ; code
      ; js_string
      ; fragment_target
      } -> (
      let ignore_because_of_js_string =
        match js_string, Config.Flag.use_js_string () with
        | Some true, false | Some false, true -> true
        | None, _ | Some true, true | Some false, false -> false
      in
      let ignore_because_of_version_constraint =
        match version_constraint with
        | [] -> false
        | l -> not (List.exists l ~f:version_match)
      in
      if ignore_because_of_version_constraint || ignore_because_of_js_string
      then `Ignored
      else
        match provides with
        | None ->
            if always
            then (
              always_included :=
                { filename; program = code; requires } :: !always_included;
              `Ok)
            else
              error
                "Found JavaScript code with neither `//Provides` nor `//Always` in file \
                 %S@."
                filename
        | Some (pi, name, kind, ka) ->
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
              let rec find_arity = function
                | [] -> None
                | ( Javascript.Function_declaration
                      (Javascript.S { Javascript.name = n; _ }, l, _, _)
                  , _ )
                  :: _
                  when String.equal name n -> Some (List.length l)
                | _ :: rem -> find_arity rem
              in
              incr last_code_id;
              let id = !last_code_id in
              let code = Macro.f code in
              let arity = find_arity code in
              let named_values = find_named_value code in
              check_primitive ~name pi ~code ~requires;
              Primitive.register name kind ka arity;
              StringSet.iter Primitive.register_named_value named_values;
              Hashtbl.add provided name { id; pi; weakdef; target_env = fragment_target };
              Hashtbl.add provided_rev id (name, pi);
              Hashtbl.add code_pieces id (code, requires);
              `Ok)

let add_file ~target_env filename =
  List.iter (parse_file filename) ~f:(fun frag ->
      let (`Ok | `Ignored) = load_fragment ~target_env ~filename frag in
      ())

let get_provided () =
  Hashtbl.fold (fun k _ acc -> StringSet.add k acc) provided StringSet.empty

let check_deps () =
  let provided = get_provided () in
  Hashtbl.iter
    (fun id (code, requires) ->
      let traverse = new Js_traverse.free in
      let _js = traverse#program code in
      let free = traverse#get_free_name in
      let requires = List.fold_right requires ~init:StringSet.empty ~f:StringSet.add in
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
    code_pieces

let load_fragments ~target_env ~filename l =
  List.iter l ~f:(fun frag ->
      let (`Ok | `Ignored) = load_fragment ~target_env ~filename frag in
      ());
  check_deps ()

let load_files ~target_env l =
  List.iter l ~f:(fun filename -> add_file ~target_env filename);
  check_deps ()

(* resolve *)
let rec resolve_dep_name_rev visited path nm =
  let id =
    try
      let x = Hashtbl.find provided nm in
      x.id
    with Not_found -> error "missing dependency '%s'@." nm
  in
  resolve_dep_id_rev visited path id

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
    let code, req = Hashtbl.find code_pieces id in
    let visited = { visited with ids = IntSet.add id visited.ids } in
    let visited =
      List.fold_left req ~init:visited ~f:(fun visited nm ->
          resolve_dep_name_rev visited path nm)
    in
    let visited = { visited with codes = code :: visited.codes } in
    visited

let init () =
  { ids = IntSet.empty; always_required_codes = List.rev !always_included; codes = [] }

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
        { state with codes = always.program :: state.codes })
  in
  let runtime = List.flatten (List.rev (program :: state.codes)) in
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
