(* Wasm_of_ocaml compiler
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

open Stdlib

let times = Debug.find "times"

module Build_info : sig
  include module type of Build_info

  val to_sexp : t -> Sexp.t

  val from_sexp : Sexp.t -> t
end = struct
  include Build_info

  let to_sexp info =
    Sexp.List
      (info
      |> to_map
      |> StringMap.bindings
      |> List.map ~f:(fun (k, v) -> Sexp.List [ Atom k; Atom v ]))

  let from_sexp info =
    let open Sexp.Util in
    info
    |> assoc
    |> List.fold_left
         ~f:(fun m (k, v) -> StringMap.add k (single string v) m)
         ~init:StringMap.empty
    |> of_map
end

module Unit_info : sig
  include module type of Unit_info

  val to_sexp : t -> Sexp.t list

  val from_sexp : Sexp.t -> t
end = struct
  include Unit_info

  let to_sexp t =
    let add nm skip v rem = if skip then rem else Sexp.List (Atom nm :: v) :: rem in
    let set nm f rem =
      add
        nm
        (List.equal ~eq:String.equal (f empty) (f t))
        (List.map ~f:(fun x -> Sexp.Atom x) (f t))
        rem
    in
    let bool nm f rem =
      add
        nm
        (Bool.equal (f empty) (f t))
        (if f t then [ Atom "true" ] else [ Atom "false" ])
        rem
    in
    []
    |> bool "effects_without_cps" (fun t -> t.effects_without_cps)
    |> set "primitives" (fun t -> t.primitives)
    |> bool "force_link" (fun t -> t.force_link)
    |> set "requires" (fun t -> StringSet.elements t.requires)
    |> set "provides" (fun t -> StringSet.elements t.provides)

  let from_sexp t =
    let open Sexp.Util in
    let opt_list l = l |> Option.map ~f:(List.map ~f:string) in
    let list default l = Option.value ~default (opt_list l) in
    let set default l =
      Option.value ~default (Option.map ~f:StringSet.of_list (opt_list l))
    in
    let bool default v = Option.value ~default (Option.map ~f:(single bool) v) in
    { provides = t |> member "provides" |> set empty.provides
    ; requires = t |> member "requires" |> set empty.requires
    ; primitives = t |> member "primitives" |> list empty.primitives
    ; aliases = []
    ; force_link = t |> member "force_link" |> bool empty.force_link
    ; effects_without_cps =
        t |> member "effects_without_cps" |> bool empty.effects_without_cps
    }
end

module Wasm_binary = struct
  let header = "\000asm\001\000\000\000"

  let check_header file ch =
    let s = really_input_string ch 8 in
    if not (String.equal s header)
    then failwith (file ^ " is not a Wasm binary file (bad magic)")

  let check ~contents = String.starts_with ~prefix:header contents

  let check_file ~file =
    let ch = open_in_bin file in
    let res =
      try
        let s = really_input_string ch 8 in
        String.equal s header
      with End_of_file -> false
    in
    close_in ch;
    res

  type t =
    { ch : in_channel
    ; limit : int
    }

  let open_in f =
    let ch = open_in_bin f in
    check_header f ch;
    { ch; limit = in_channel_length ch }

  let from_channel ~name ch pos len =
    seek_in ch pos;
    check_header name ch;
    { ch; limit = pos + len }

  let rec read_uint ?(n = 5) ch =
    let i = input_byte ch in
    if n = 1 then assert (i < 16);
    if i < 128 then i else i - 128 + (read_uint ~n:(n - 1) ch lsl 7)

  let rec read_sint ?(n = 5) ch =
    let i = input_byte ch in
    if n = 1 then assert (i < 8 || (i > 120 && i < 128));
    if i < 64
    then i
    else if i < 128
    then i - 128
    else i - 128 + (read_sint ~n:(n - 1) ch lsl 7)

  type section =
    { id : int
    ; size : int
    }

  let next_section ch =
    if pos_in ch.ch = ch.limit
    then None
    else
      let id = input_byte ch.ch in
      let size = read_uint ch.ch in
      Some { id; size }

  let skip_section ch { size; _ } = seek_in ch.ch (pos_in ch.ch + size)

  let vec f ch =
    let rec loop acc n = if n = 0 then List.rev acc else loop (f ch :: acc) (n - 1) in
    loop [] (read_uint ch)

  let name ch =
    let n = read_uint ch in
    really_input_string ch n

  let heaptype ch = ignore (read_sint ch)

  let reftype' i ch =
    match i with
    | 0x6a | 0x6b | 0x6c | 0x6d | 0x6e | 0x6f | 0x70 | 0x71 | 0x72 | 0x73 -> ()
    | 0x63 | 0x64 -> heaptype ch
    | _ ->
        Format.eprintf "Unknown reftype %x@." i;
        assert false

  let reftype ch = reftype' (input_byte ch) ch

  let valtype ch =
    let i = read_uint ch in
    match i with
    | 0x7b | 0x7c | 0x7d | 0x7e | 0x7f -> ()
    | _ -> reftype' i ch

  let limits ch =
    match input_byte ch with
    | 0 -> ignore (read_uint ch)
    | 1 ->
        ignore (read_uint ch);
        ignore (read_uint ch)
    | _ -> assert false

  let memtype = limits

  let tabletype ch =
    reftype ch;
    limits ch

  type import =
    { module_ : string
    ; name : string
    }

  let import ch =
    let module_ = name ch in
    let name = name ch in
    let d = read_uint ch in
    let _ =
      match d with
      | 0 -> ignore (read_uint ch)
      | 1 -> tabletype ch
      | 2 -> memtype ch
      | 3 ->
          let _typ = valtype ch in
          let _mut = input_byte ch in
          ()
      | 4 ->
          assert (read_uint ch = 0);
          ignore (read_uint ch)
      | _ ->
          Format.eprintf "Unknown import %x@." d;
          assert false
    in
    { module_; name }

  let export ch =
    let name = name ch in
    let d = read_uint ch in
    if d > 4
    then (
      Format.eprintf "Unknown export %x@." d;
      assert false);
    ignore (read_uint ch);
    name

  let read_imports ~file =
    let ch = open_in file in
    let rec find_section () =
      match next_section ch with
      | None -> false
      | Some s ->
          s.id = 2
          ||
          (skip_section ch s;
           find_section ())
    in
    let res = if find_section () then vec import ch.ch else [] in
    close_in ch.ch;
    res

  type interface =
    { imports : import list
    ; exports : string list
    }

  let read_interface ch =
    let rec find_sections i =
      match next_section ch with
      | None -> i
      | Some s ->
          if s.id = 2
          then find_sections { i with imports = vec import ch.ch }
          else if s.id = 7
          then { i with exports = vec export ch.ch }
          else (
            skip_section ch s;
            find_sections i)
    in
    find_sections { imports = []; exports = [] }

  let append_source_map_section ~file ~url =
    let ch = open_out_gen [ Open_wronly; Open_append; Open_binary ] 0o666 file in
    let rec output_uint buf i =
      if i < 128
      then Buffer.add_char buf (Char.chr i)
      else (
        Buffer.add_char buf (Char.chr (128 + (i land 127)));
        output_uint buf (i lsr 7))
    in
    let buf = Buffer.create 16 in
    let output_name buf s =
      output_uint buf (String.length s);
      Buffer.add_string buf s
    in
    output_name buf "sourceMappingURL";
    output_name buf url;
    let section_contents = Buffer.contents buf in
    Buffer.clear buf;
    Buffer.add_char buf '\000';
    output_uint buf (String.length section_contents);
    output_string ch (Buffer.contents buf);
    output_string ch section_contents;
    close_out ch
end

let trim_semi s =
  let l = ref (String.length s) in
  while
    !l > 0
    &&
    match s.[!l - 1] with
    | ';' | '\n' -> true
    | _ -> false
  do
    decr l
  done;
  String.sub s ~pos:0 ~len:!l

type unit_data =
  { unit_name : string
  ; unit_info : Unit_info.t
  ; strings : string list
  ; fragments : (string * Javascript.expression) list
  }

let info_to_sexp ~predefined_exceptions ~build_info ~unit_data =
  let add nm skip v rem = if skip then rem else Sexp.List (Atom nm :: v) :: rem in
  let units =
    List.map
      ~f:(fun { unit_name; unit_info; strings; fragments } ->
        Sexp.List
          (Unit_info.to_sexp unit_info
          |> add "name" false [ Atom unit_name ]
          |> add
               "strings"
               (List.is_empty strings)
               (List.map ~f:(fun s -> Sexp.Atom s) strings)
          |> add
               "fragments"
               (List.is_empty fragments)
               [ Sexp.Atom (Base64.encode_string (Marshal.to_string fragments [])) ]))
      unit_data
  in
  Sexp.List
    ([]
    |> add
         "predefined_exceptions"
         (StringSet.is_empty predefined_exceptions)
         (List.map ~f:(fun s -> Sexp.Atom s) (StringSet.elements predefined_exceptions))
    |> add "units" (List.is_empty unit_data) units
    |> add "build_info" false [ Build_info.to_sexp build_info ])

let info_from_sexp info =
  let open Sexp.Util in
  let build_info =
    info |> member "build_info" |> mandatory (single Build_info.from_sexp)
  in
  let predefined_exceptions =
    info
    |> member "predefined_exceptions"
    |> Option.value ~default:[]
    |> List.map ~f:string
    |> StringSet.of_list
  in
  let unit_data =
    info
    |> member "units"
    |> Option.value ~default:[]
    |> List.map ~f:(fun u ->
           let unit_info = u |> Unit_info.from_sexp in
           let unit_name =
             u |> member "name" |> Option.value ~default:[] |> single string
           in
           let strings =
             u |> member "strings" |> Option.value ~default:[] |> List.map ~f:string
           in
           let fragments =
             u
             |> member "fragments"
             |> Option.map ~f:(single string)
             |> Option.map ~f:(fun s -> Marshal.from_string (Base64.decode_exn s) 0)
             |> Option.value ~default:[]
             (*
                           |> to_option to_assoc
                           |> Option.value ~default:[]
                           |> List.map ~f:(fun (nm, e) ->
                                  ( nm
                                  , let lex = Parse_js.Lexer.of_string (to_string e) in
                                    Parse_js.parse_expr lex ))*)
           in
           { unit_name; unit_info; strings; fragments })
  in
  build_info, predefined_exceptions, unit_data

let add_info z ?(predefined_exceptions = StringSet.empty) ~build_info ~unit_data () =
  Zip.add_entry
    z
    ~name:"info.sexp"
    ~contents:
      (Sexp.to_string (info_to_sexp ~predefined_exceptions ~build_info ~unit_data))

let read_info z = info_from_sexp (Sexp.from_string (Zip.read_entry z ~name:"info.sexp"))

let generate_start_function ~to_link ~out_file =
  let t1 = Timer.make () in
  Filename.gen_file out_file
  @@ fun ch ->
  let context = Generate.start () in
  Generate.add_init_function ~context ~to_link:("prelude" :: to_link);
  Generate.wasm_output ch ~opt_source_map_file:None ~context;
  if times () then Format.eprintf "    generate start: %a@." Timer.print t1

let output_js js =
  let js = Driver.simplify_js js in
  let js = Driver.name_variables js in
  Code.Var.reset ();
  let b = Buffer.create 1024 in
  let f = Pretty_print.to_buffer b in
  Driver.configure f;
  ignore (Js_output.program f js);
  Buffer.contents b

let report_missing_primitives missing =
  if not (List.is_empty missing)
  then (
    warn "There are some missing Wasm primitives@.";
    warn "Dummy implementations (raising an exception) ";
    warn "will be provided.@.";
    warn "Missing primitives:@.";
    List.iter ~f:(fun nm -> warn "  %s@." nm) missing)

let build_runtime_arguments
    ~link_spec
    ~separate_compilation
    ~missing_primitives
    ~wasm_dir
    ~generated_js
    () =
  let missing_primitives = if Config.Flag.genprim () then missing_primitives else [] in
  if not separate_compilation then report_missing_primitives missing_primitives;
  let obj l =
    Javascript.EObj
      (List.map
         ~f:(fun (nm, v) ->
           let id = Utf8_string.of_string_exn nm in
           Javascript.Property (PNS id, v))
         l)
  in
  let generated_js =
    List.concat
    @@ List.map
         ~f:(fun (unit_name, (strings, fragments)) ->
           let name s =
             match unit_name with
             | None -> s
             | Some nm -> nm ^ "." ^ s
           in
           let strings =
             if List.is_empty strings
             then []
             else
               [ ( name "strings"
                 , Javascript.EArr
                     (List.map
                        ~f:(fun s ->
                          Javascript.Element (EStr (Utf8_string.of_string_exn s)))
                        strings) )
               ]
           in
           let fragments =
             if List.is_empty fragments then [] else [ name "fragments", obj fragments ]
           in
           strings @ fragments)
         generated_js
  in
  let generated_js =
    if not (List.is_empty missing_primitives)
    then
      ( "env"
      , obj
          (List.map
             ~f:(fun nm ->
               ( nm
               , Javascript.EArrow
                   ( Javascript.fun_
                       []
                       [ ( Throw_statement
                             (ENew
                                ( EVar
                                    (Javascript.ident (Utf8_string.of_string_exn "Error"))
                                , Some
                                    [ Arg
                                        (EStr
                                           (Utf8_string.of_string_exn
                                              (nm ^ " not implemented")))
                                    ]
                                , N ))
                         , N )
                       ]
                       N
                   , false
                   , AUnknown ) ))
             missing_primitives) )
      :: generated_js
    else generated_js
  in
  let generated_js =
    if List.is_empty generated_js
    then obj generated_js
    else
      let var ident e =
        Javascript.variable_declaration [ Javascript.ident ident, (e, N) ], Javascript.N
      in
      Javascript.call
        (EArrow
           ( Javascript.fun_
               [ Javascript.ident Global_constant.global_object_ ]
               [ var
                   Global_constant.old_global_object_
                   (EVar (Javascript.ident Global_constant.global_object_))
               ; var
                   Global_constant.exports_
                   (EBin
                      ( Or
                      , EDot
                          ( EDot
                              ( EVar (Javascript.ident Global_constant.global_object_)
                              , ANullish
                              , Utf8_string.of_string_exn "module" )
                          , ANullish
                          , Utf8_string.of_string_exn "export" )
                      , EVar (Javascript.ident Global_constant.global_object_) ))
               ; Return_statement (Some (obj generated_js), N), N
               ]
               N
           , true
           , AUnknown ))
        [ EVar (Javascript.ident Global_constant.global_object_) ]
        N
  in
  obj
    [ ( "link"
      , EArr
          (List.map
             ~f:(fun (m, deps) ->
               Javascript.Element
                 (EArr
                    [ Element (EStr (Utf8_string.of_string_exn m))
                    ; Element
                        (match deps with
                        | None ->
                            ENum (Javascript.Num.of_targetint (Targetint.of_int_exn 0))
                        | Some l ->
                            EArr
                              (List.map
                                 ~f:(fun i ->
                                   Javascript.Element
                                     (ENum
                                        (Javascript.Num.of_targetint
                                           (Targetint.of_int_exn i))))
                                 l))
                    ]))
             link_spec) )
    ; "generated", generated_js
    ; "src", EStr (Utf8_string.of_string_exn (Filename.basename wasm_dir))
    ]

let source_name i j file =
  let prefix =
    match i, j with
    | None, None -> "src-"
    | Some i, None -> Printf.sprintf "src-%d-" i
    | None, Some j -> Printf.sprintf "src-%d-" j
    | Some i, Some j -> Printf.sprintf "src-%d.%d-" i j
  in
  prefix ^ Filename.basename file ^ ".json"

let extract_source_map ~dir ~name z =
  if Zip.has_entry z ~name:"source_map.map"
  then (
    let sm = Source_map.of_string (Zip.read_entry z ~name:"source_map.map") in
    let sm =
      Wasm_source_map.insert_source_contents sm (fun i j file ->
          let name = source_name i j file in
          if Zip.has_entry z ~name then Some (Zip.read_entry z ~name) else None)
    in
    let map_name = name ^ ".wasm.map" in
    Source_map.to_file sm (Filename.concat dir map_name);
    Wasm_binary.append_source_map_section
      ~file:(Filename.concat dir (name ^ ".wasm"))
      ~url:map_name)

let link_to_directory ~files_to_link ~files ~enable_source_maps ~dir =
  let process_file z ~name ~name' =
    let ch, pos, len, crc = Zip.get_entry z ~name:(name ^ ".wasm") in
    let intf = Wasm_binary.read_interface (Wasm_binary.from_channel ~name ch pos len) in
    let name' = Printf.sprintf "%s-%08lx" name' crc in
    Zip.extract_file
      z
      ~name:(name ^ ".wasm")
      ~file:(Filename.concat dir (name' ^ ".wasm"));
    name', intf
  in
  let z = Zip.open_in (fst (List.hd files)) in
  let runtime, runtime_intf = process_file z ~name:"runtime" ~name':"runtime" in
  let prelude, _ = process_file z ~name:"prelude" ~name':"prelude" in
  Zip.close_in z;
  let lst =
    List.tl files
    |> List.map ~f:(fun (file, _) ->
           if StringSet.mem file files_to_link
           then (
             let z = Zip.open_in file in
             let name' = file |> Filename.basename |> Filename.remove_extension in
             let ((name', _) as res) = process_file z ~name:"code" ~name' in
             if enable_source_maps then extract_source_map ~dir ~name:name' z;
             Zip.close_in z;
             Some res)
           else None)
    |> List.filter_map ~f:(fun x -> x)
  in
  runtime :: prelude :: List.map ~f:fst lst, (runtime_intf, List.map ~f:snd lst)

let compute_dependencies ~files_to_link ~files =
  let h = String.Hashtbl.create 128 in
  let i = ref 2 in
  List.filter_map
    ~f:(fun (file, (_, units)) ->
      if StringSet.mem file files_to_link
      then (
        let s =
          List.fold_left
            ~f:(fun s { unit_info; _ } ->
              StringSet.fold
                (fun unit_name s ->
                  try IntSet.add (String.Hashtbl.find h unit_name) s with Not_found -> s)
                unit_info.requires
                s)
            ~init:IntSet.empty
            units
        in
        List.iter ~f:(fun { unit_name; _ } -> String.Hashtbl.add h unit_name !i) units;
        incr i;
        Some (Some (IntSet.elements s)))
      else None)
    (List.tl files)

let compute_missing_primitives (runtime_intf, intfs) =
  let provided_primitives = StringSet.of_list runtime_intf.Wasm_binary.exports in
  StringSet.elements
  @@ List.fold_left
       ~f:(fun s { Wasm_binary.imports; _ } ->
         List.fold_left
           ~f:(fun s { Wasm_binary.module_; name; _ } ->
             if String.equal module_ "env" && not (StringSet.mem name provided_primitives)
             then StringSet.add name s
             else s)
           ~init:s
           imports)
       ~init:StringSet.empty
       intfs

let load_information files =
  match files with
  | [] -> assert false
  | runtime :: other_files ->
      let build_info, predefined_exceptions, _unit_data =
        Zip.with_open_in runtime read_info
      in
      ( predefined_exceptions
      , (runtime, (build_info, []))
        :: List.map other_files ~f:(fun file ->
               let build_info, _predefined_exceptions, unit_data =
                 Zip.with_open_in file read_info
               in
               file, (build_info, unit_data)) )

let remove_directory path =
  try
    let files = Sys.readdir path in
    Array.iter ~f:(fun file -> Sys.remove (Filename.concat path file)) files;
    Sys.rmdir path (* Since OCaml 4.12, so we cannot put this in fs.ml *)
  with Sys_error _ -> ()

let gen_dir dir f =
  let d_tmp = Filename.temp_file_name ~temp_dir:(Filename.dirname dir) "assets" ".tmp" in
  try
    let res = f d_tmp in
    remove_directory dir;
    Sys.rename d_tmp dir;
    res
  with exc ->
    remove_directory d_tmp;
    raise exc

let link ~output_file ~linkall ~enable_source_maps ~files =
  if times () then Format.eprintf "linking@.";
  let t = Timer.make () in
  let predefined_exceptions, files = load_information files in
  (match files with
  | [] -> assert false
  | (file, (bi, _)) :: r ->
      (match Build_info.kind bi with
      | `Runtime -> ()
      | _ ->
          failwith
            "The first input file should be a runtime built using 'wasm_of_ocaml \
             build-runtime'.");
      Build_info.configure bi;
      ignore
        (List.fold_left
           ~init:bi
           ~f:(fun bi (file', (bi', _)) ->
             (match Build_info.kind bi' with
             | `Runtime ->
                 failwith "The runtime file should be listed first on the command line."
             | _ -> ());
             Build_info.merge file bi file' bi')
           r));
  if times () then Format.eprintf "    reading information: %a@." Timer.print t;
  let t1 = Timer.make () in
  let missing, files_to_link =
    List.fold_right
      files
      ~init:(StringSet.empty, StringSet.empty)
      ~f:(fun (file, (build_info, units)) (requires, files_to_link) ->
        let cmo_file =
          match Build_info.kind build_info with
          | `Cmo -> true
          | `Cma | `Exe | `Runtime | `Unknown -> false
        in
        if
          (not (Config.Flag.auto_link ()))
          || cmo_file
          || linkall
          || List.exists ~f:(fun { unit_info; _ } -> unit_info.force_link) units
          || List.exists
               ~f:(fun { unit_info; _ } ->
                 not (StringSet.is_empty (StringSet.inter requires unit_info.provides)))
               units
        then
          ( List.fold_right units ~init:requires ~f:(fun { unit_info; _ } requires ->
                StringSet.diff
                  (StringSet.union unit_info.requires requires)
                  unit_info.provides)
          , StringSet.add file files_to_link )
        else requires, files_to_link)
  in
  let _, to_link =
    List.fold_right
      files
      ~init:(StringSet.empty, [])
      ~f:(fun (_file, (build_info, units)) acc ->
        let cmo_file =
          match Build_info.kind build_info with
          | `Cmo -> true
          | `Cma | `Exe | `Runtime | `Unknown -> false
        in
        List.fold_right
          units
          ~init:acc
          ~f:(fun { unit_name; unit_info; _ } (requires, to_link) ->
            if
              (not (Config.Flag.auto_link ()))
              || cmo_file
              || linkall
              || unit_info.force_link
              || not (StringSet.is_empty (StringSet.inter requires unit_info.provides))
            then
              ( StringSet.diff
                  (StringSet.union unit_info.requires requires)
                  unit_info.provides
              , unit_name :: to_link )
            else requires, to_link))
  in
  let missing = StringSet.diff missing predefined_exceptions in
  if not (StringSet.is_empty missing)
  then
    failwith
      (Printf.sprintf
         "Could not find compilation unit for %s"
         (String.concat ~sep:", " (StringSet.elements missing)));
  if times () then Format.eprintf "    finding what to link: %a@." Timer.print t1;
  if times () then Format.eprintf "  scan: %a@." Timer.print t;
  let t = Timer.make () in
  let interfaces, wasm_dir, link_spec =
    let dir = Filename.chop_extension output_file ^ ".assets" in
    gen_dir dir
    @@ fun tmp_dir ->
    Sys.mkdir tmp_dir 0o777;
    let start_module =
      "start-"
      ^ String.sub
          (Digest.to_hex (Digest.string (String.concat ~sep:"/" to_link)))
          ~pos:0
          ~len:8
    in
    generate_start_function
      ~to_link
      ~out_file:(Filename.concat tmp_dir (start_module ^ ".wasm"));
    let module_names, interfaces =
      link_to_directory ~files_to_link ~files ~enable_source_maps ~dir:tmp_dir
    in
    ( interfaces
    , dir
    , let to_link = compute_dependencies ~files_to_link ~files in
      List.combine module_names (None :: None :: to_link) @ [ start_module, None ] )
  in
  let missing_primitives = compute_missing_primitives interfaces in
  if times () then Format.eprintf "    copy wasm files: %a@." Timer.print t;
  let t1 = Timer.make () in
  let js_runtime =
    match files with
    | (file, _) :: _ ->
        Zip.with_open_in file (fun z -> Zip.read_entry z ~name:"runtime.js")
    | _ -> assert false
  in
  let generated_js =
    List.concat
    @@ List.map files ~f:(fun (_, (_, units)) ->
           List.map units ~f:(fun { unit_name; strings; fragments; _ } ->
               Some unit_name, (strings, fragments)))
  in
  let runtime_args =
    let js =
      build_runtime_arguments
        ~link_spec
        ~separate_compilation:true
        ~missing_primitives
        ~wasm_dir
        ~generated_js
        ()
    in
    output_js [ Javascript.Expression_statement js, Javascript.N ]
  in
  Fs.gen_file output_file
  @@ fun tmp_output_file ->
  Fs.write_file
    ~name:tmp_output_file
    ~contents:(trim_semi js_runtime ^ "\n" ^ runtime_args);
  if times () then Format.eprintf "    build JS runtime: %a@." Timer.print t1;
  if times () then Format.eprintf "  emit: %a@." Timer.print t

let rec get_source_map_files ~tmp_buf files src_index =
  let z = Zip.open_in files.(!src_index) in
  incr src_index;
  let l = ref [] in
  (if Zip.has_entry z ~name:"source_map.map"
   then
     let data = Zip.read_entry z ~name:"source_map.map" in
     let sm = Source_map.Standard.of_string ~tmp_buf data in
     if not (Wasm_source_map.is_empty sm)
     then
       Wasm_source_map.iter_sources (Standard sm) (fun i j file ->
           l := source_name i j file :: !l));
  if not (List.is_empty !l)
  then z, Array.of_list (List.rev !l)
  else (
    Zip.close_in z;
    get_source_map_files ~tmp_buf files src_index)

let add_source_map files z sm =
  let tmp_buf = Buffer.create 10000 in
  Zip.add_entry z ~name:"source_map.map" ~contents:(Source_map.to_string sm);
  let files = Array.of_list files in
  let src_index = ref 0 in
  let st = ref None in
  let finalize () =
    match !st with
    | Some (_, (z', _)) -> Zip.close_in z'
    | None -> ()
  in
  Wasm_source_map.iter_sources sm (fun i j file ->
      let z', files =
        match !st with
        | Some (i', st) when Option.equal ( = ) i i' -> st
        | _ ->
            let st' = get_source_map_files ~tmp_buf files src_index in
            finalize ();
            st := Some (i, st');
            st'
      in
      if Array.length files > 0 (* Source has source map *)
      then
        let name = files.(Option.value ~default:0 j) in
        if Zip.has_entry z' ~name
        then Zip.copy_file z' z ~src_name:name ~dst_name:(source_name i j file));
  finalize ()

let make_library ~output_file ~enable_source_maps ~files =
  let info =
    List.map files ~f:(fun file ->
        let build_info, _predefined_exceptions, unit_data =
          Zip.with_open_in file read_info
        in
        (match Build_info.kind build_info with
        | `Cmo -> ()
        | `Runtime | `Cma | `Exe | `Unknown ->
            failwith (Printf.sprintf "File '%s' is not a .wasmo file." file));
        file, build_info, unit_data)
  in
  let build_info =
    Build_info.with_kind
      (match info with
      | (file, bi, _) :: r ->
          Build_info.configure bi;
          List.fold_left
            ~init:bi
            ~f:(fun bi (file', bi', _) -> Build_info.merge file bi file' bi')
            r
      | [] -> Build_info.create `Cma)
      `Cma
  in
  let unit_data = List.concat (List.map ~f:(fun (_, _, unit_data) -> unit_data) info) in
  Fs.gen_file output_file
  @@ fun tmp_output_file ->
  let z = Zip.open_out tmp_output_file in
  add_info z ~build_info ~unit_data ();
  Fs.with_intermediate_file (Filename.temp_file "wasm" ".wasm")
  @@ fun tmp_wasm_file ->
  let output_sourcemap =
    Wasm_link.f
      (let tmp_buf = Buffer.create 10000 in
       List.map
         ~f:(fun file ->
           let z' = Zip.open_in file in
           { Wasm_link.module_name = "OCaml"
           ; file
           ; code = Some (Zip.read_entry z' ~name:"code.wasm")
           ; opt_source_map =
               (if enable_source_maps && Zip.has_entry z' ~name:"source_map.map"
                then
                  Some
                    (Source_map.Standard.of_string
                       ~tmp_buf
                       (Zip.read_entry z' ~name:"source_map.map"))
                else None)
           })
         files)
      ~output_file:tmp_wasm_file
  in
  Zip.add_file z ~name:"code.wasm" ~file:tmp_wasm_file;
  if enable_source_maps then add_source_map files z output_sourcemap;
  Zip.close_out z

let link ~output_file ~linkall ~mklib ~enable_source_maps ~files =
  try
    if mklib
    then make_library ~output_file ~enable_source_maps ~files
    else link ~output_file ~linkall ~enable_source_maps ~files
  with Build_info.Incompatible_build_info { key; first = f1, v1; second = f2, v2 } ->
    let string_of_v = function
      | None -> "<empty>"
      | Some v -> v
    in
    failwith
      (Printf.sprintf
         "Incompatible build info detected while linking.\n - %s: %s=%s\n - %s: %s=%s"
         f1
         key
         (string_of_v v1)
         f2
         key
         (string_of_v v2))
