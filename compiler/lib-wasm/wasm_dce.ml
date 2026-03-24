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

(* Reference collector: scans wasm bytecode to find all referenced indices
   without rewriting. Mirrors Wasm_link.Scan.scanner's opcode dispatch. *)

type refs =
  { mutable funcs : int list
  ; mutable types : int list
  ; mutable globals : int list
  ; mutable elems : int list
  ; mutable datas : int list
  ; mutable tags : int list
  }

let create_refs () =
  { funcs = []; types = []; globals = []; elems = []; datas = []; tags = [] }

let collector code =
  let get pos = Char.code (String.get code pos) in
  let rec int pos = if get pos >= 128 then int (pos + 1) else pos + 1 in
  let rec uint32 pos =
    let i = get pos in
    if i < 128
    then pos + 1, i
    else
      let pos, i' = pos + 1 |> uint32 in
      pos, (i' lsl 7) + (i land 0x7f)
  in
  let rec sint32 pos =
    let i = get pos in
    if i < 64
    then pos + 1, i
    else if i < 128
    then pos + 1, i - 128
    else
      let pos, i' = pos + 1 |> sint32 in
      pos, i - 128 + (i' lsl 7)
  in
  let rec repeat n f pos = if n = 0 then pos else repeat (n - 1) f (f pos) in
  let vector f pos =
    let pos, i =
      let i = get pos in
      if i < 128 then pos + 1, i else uint32 pos
    in
    repeat i f pos
  in
  let _name pos =
    let pos', i =
      let i = get pos in
      if i < 128 then pos + 1, i else uint32 pos
    in
    pos' + i
  in
  let collect_uint pos =
    let pos', idx =
      let i = get pos in
      if i < 128
      then pos + 1, i
      else
        let i' = get (pos + 1) in
        if i' < 128 then pos + 2, (i' lsl 7) + (i land 0x7f) else uint32 pos
    in
    pos', idx
  in
  let collect_sint pos =
    let pos', idx =
      let i = get pos in
      if i < 64 then pos + 1, i else if i < 128 then pos + 1, i - 128 else sint32 pos
    in
    pos', idx
  in
  let funcidx refs pos =
    let pos', idx = collect_uint pos in
    refs.funcs <- idx :: refs.funcs;
    pos'
  in
  let typeidx refs pos =
    let pos', idx = collect_uint pos in
    refs.types <- idx :: refs.types;
    pos'
  in
  let signed_typeidx refs pos =
    let pos', idx = collect_sint pos in
    refs.types <- idx :: refs.types;
    pos'
  in
  let tableidx _refs pos = int pos in
  let memidx _refs pos = int pos in
  let globalidx refs pos =
    let pos', idx = collect_uint pos in
    refs.globals <- idx :: refs.globals;
    pos'
  in
  let elemidx refs pos =
    let pos', idx = collect_uint pos in
    refs.elems <- idx :: refs.elems;
    pos'
  in
  let dataidx refs pos =
    let pos', idx = collect_uint pos in
    refs.datas <- idx :: refs.datas;
    pos'
  in
  let tagidx refs pos =
    let pos', idx = collect_uint pos in
    refs.tags <- idx :: refs.tags;
    pos'
  in
  let labelidx = int in
  let localidx = int in
  let laneidx pos = pos + 1 in
  let heaptype refs pos =
    let c = get pos in
    if c >= 64 && c < 128 then pos + 1 else signed_typeidx refs pos
  in
  let absheaptype pos =
    match get pos with
    | 0x73 | 0x72 | 0x71 | 0x70 | 0x6F | 0x6E | 0x6D | 0x6C | 0x6B | 0x6A -> pos + 1
    | c -> failwith (Printf.sprintf "Bad heap type 0x%02X" c)
  in
  let reftype refs pos =
    match get pos with
    | 0x63 | 0x64 -> pos + 1 |> heaptype refs
    | _ -> pos |> absheaptype
  in
  let valtype refs pos =
    let c = get pos in
    match c with
    | 0x63 | 0x64 -> pos + 1 |> heaptype refs
    | _ -> pos + 1
  in
  let blocktype refs pos =
    let c = get pos in
    if c >= 64 && c < 128 then pos |> valtype refs else pos |> signed_typeidx refs
  in
  let memarg _refs pos =
    let pos', c = uint32 pos in
    if c < 64 then pos' |> int else pos' |> int |> int
  in
  let rec instructions refs pos =
    match get pos with
    (* Control instruction *)
    | 0x00 | 0x01 | 0x0F -> pos + 1 |> instructions refs
    | 0x02 | 0x03 ->
        pos + 1 |> blocktype refs |> instructions refs |> block_end |> instructions refs
    | 0x04 -> pos + 1 |> blocktype refs |> instructions refs |> opt_else refs
    | 0x0C | 0x0D | 0xD5 | 0xD6 -> pos + 1 |> labelidx |> instructions refs
    | 0x0E -> pos + 1 |> vector labelidx |> labelidx |> instructions refs
    | 0x10 | 0x12 -> pos + 1 |> funcidx refs |> instructions refs
    | 0x11 | 0x13 -> pos + 1 |> typeidx refs |> tableidx refs |> instructions refs
    | 0x14 | 0x15 -> pos + 1 |> typeidx refs |> instructions refs
    (* Exceptions *)
    | 0x06 -> pos + 1 |> blocktype refs |> instructions refs |> opt_catch refs
    | 0x08 -> pos + 1 |> tagidx refs |> instructions refs
    | 0x09 -> pos + 1 |> int |> instructions refs
    | 0x0A -> pos + 1 |> instructions refs
    (* Parametric instructions *)
    | 0x1A | 0x1B -> pos + 1 |> instructions refs
    | 0x1C -> pos + 1 |> vector (valtype refs) |> instructions refs
    | 0x1F ->
        pos + 1
        |> blocktype refs
        |> vector (catch refs)
        |> instructions refs
        |> block_end
        |> instructions refs
    (* Variable instructions *)
    | 0x20 | 0x21 | 0x22 -> pos + 1 |> localidx |> instructions refs
    | 0x23 | 0x24 -> pos + 1 |> globalidx refs |> instructions refs
    (* Table instructions *)
    | 0x25 | 0x26 -> pos + 1 |> tableidx refs |> instructions refs
    (* Memory instructions *)
    | 0x28
    | 0x29
    | 0x2A
    | 0x2B
    | 0x2C
    | 0x2D
    | 0x2E
    | 0x2F
    | 0x30
    | 0x31
    | 0x32
    | 0x33
    | 0x34
    | 0x35
    | 0x36
    | 0x37
    | 0x38
    | 0x39
    | 0x3A
    | 0x3B
    | 0x3C
    | 0x3D
    | 0x3E -> pos + 1 |> memarg refs |> instructions refs
    | 0x3F | 0x40 -> pos + 1 |> memidx refs |> instructions refs
    (* Numeric instructions *)
    | 0x41 | 0x42 -> pos + 1 |> int |> instructions refs
    | 0x43 -> pos + 5 |> instructions refs
    | 0x44 -> pos + 9 |> instructions refs
    | 0x45
    | 0x46
    | 0x47
    | 0x48
    | 0x49
    | 0x4A
    | 0x4B
    | 0x4C
    | 0x4D
    | 0x4E
    | 0x4F
    | 0x50
    | 0x51
    | 0x52
    | 0x53
    | 0x54
    | 0x55
    | 0x56
    | 0x57
    | 0x58
    | 0x59
    | 0x5A
    | 0x5B
    | 0x5C
    | 0x5D
    | 0x5E
    | 0x5F
    | 0x60
    | 0x61
    | 0x62
    | 0x63
    | 0x64
    | 0x65
    | 0x66
    | 0x67
    | 0x68
    | 0x69
    | 0x6A
    | 0x6B
    | 0x6C
    | 0x6D
    | 0x6E
    | 0x6F
    | 0x70
    | 0x71
    | 0x72
    | 0x73
    | 0x74
    | 0x75
    | 0x76
    | 0x77
    | 0x78
    | 0x79
    | 0x7A
    | 0x7B
    | 0x7C
    | 0x7D
    | 0x7E
    | 0x7F
    | 0x80
    | 0x81
    | 0x82
    | 0x83
    | 0x84
    | 0x85
    | 0x86
    | 0x87
    | 0x88
    | 0x89
    | 0x8A
    | 0x8B
    | 0x8C
    | 0x8D
    | 0x8E
    | 0x8F
    | 0x90
    | 0x91
    | 0x92
    | 0x93
    | 0x94
    | 0x95
    | 0x96
    | 0x97
    | 0x98
    | 0x99
    | 0x9A
    | 0x9B
    | 0x9C
    | 0x9D
    | 0x9E
    | 0x9F
    | 0xA0
    | 0xA1
    | 0xA2
    | 0xA3
    | 0xA4
    | 0xA5
    | 0xA6
    | 0xA7
    | 0xA8
    | 0xA9
    | 0xAA
    | 0xAB
    | 0xAC
    | 0xAD
    | 0xAE
    | 0xAF
    | 0xB0
    | 0xB1
    | 0xB2
    | 0xB3
    | 0xB4
    | 0xB5
    | 0xB6
    | 0xB7
    | 0xB8
    | 0xB9
    | 0xBA
    | 0xBB
    | 0xBC
    | 0xBD
    | 0xBE
    | 0xBF
    | 0xC0
    | 0xC1
    | 0xC2
    | 0xC3
    | 0xC4 -> pos + 1 |> instructions refs
    (* Reference instructions *)
    | 0xD0 -> pos + 1 |> heaptype refs |> instructions refs
    | 0xD1 | 0xD3 | 0xD4 -> pos + 1 |> instructions refs
    | 0xD2 -> pos + 1 |> funcidx refs |> instructions refs
    | 0xFB -> pos + 1 |> gc_instruction refs
    | 0xFC -> (
        match get (pos + 1) with
        | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> pos + 2 |> instructions refs
        | 8 -> pos + 2 |> dataidx refs |> memidx refs |> instructions refs
        | 9 -> pos + 2 |> dataidx refs |> instructions refs
        | 10 -> pos + 2 |> memidx refs |> memidx refs |> instructions refs
        | 11 -> pos + 2 |> memidx refs |> instructions refs
        | 12 -> pos + 2 |> elemidx refs |> tableidx refs |> instructions refs
        | 13 -> pos + 2 |> elemidx refs |> instructions refs
        | 14 -> pos + 2 |> tableidx refs |> tableidx refs |> instructions refs
        | 15 | 16 | 17 -> pos + 2 |> tableidx refs |> instructions refs
        | c -> failwith (Printf.sprintf "Bad instruction 0xFC 0x%02X" c))
    | 0xFD -> pos + 1 |> vector_instruction refs
    | 0xFE -> pos + 1 |> atomic_instruction refs
    | _ -> pos
  and gc_instruction refs pos =
    match get pos with
    | 0 | 1 | 6 | 7 | 11 | 12 | 13 | 14 | 16 ->
        pos + 1 |> typeidx refs |> instructions refs
    | 2 | 3 | 4 | 5 | 8 -> pos + 1 |> typeidx refs |> int |> instructions refs
    | 9 | 18 -> pos + 1 |> typeidx refs |> dataidx refs |> instructions refs
    | 10 | 19 -> pos + 1 |> typeidx refs |> elemidx refs |> instructions refs
    | 15 | 26 | 27 | 28 | 29 | 30 -> pos + 1 |> instructions refs
    | 17 -> pos + 1 |> typeidx refs |> typeidx refs |> instructions refs
    | 20 | 21 | 22 | 23 -> pos + 1 |> heaptype refs |> instructions refs
    | 24 | 25 ->
        pos + 2 |> labelidx |> heaptype refs |> heaptype refs |> instructions refs
    | c -> failwith (Printf.sprintf "Bad instruction 0xFB 0x%02X" c)
  and vector_instruction refs pos =
    let pos, i = uint32 pos in
    match i with
    | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 92 | 93 ->
        pos + 1 |> memarg refs |> instructions refs
    | 84 | 85 | 86 | 87 | 88 | 89 | 90 | 91 ->
        pos + 1 |> memarg refs |> laneidx |> instructions refs
    | 12 | 13 -> pos + 17 |> instructions refs
    | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 ->
        pos + 1 |> laneidx |> instructions refs
    | ( 162
      | 165
      | 166
      | 175
      | 176
      | 178
      | 179
      | 180
      | 187
      | 194
      | 197
      | 198
      | 207
      | 208
      | 210
      | 211
      | 212
      | 226
      | 238 ) as c -> failwith (Printf.sprintf "Bad instruction 0xFD 0x%02X" c)
    | c ->
        if c <= 275
        then pos + 1 |> instructions refs
        else failwith (Printf.sprintf "Bad instruction 0xFD 0x%02X" c)
  and atomic_instruction refs pos =
    match get pos with
    | 0
    | 1
    | 2
    | 16
    | 17
    | 18
    | 19
    | 20
    | 21
    | 22
    | 23
    | 24
    | 25
    | 26
    | 27
    | 28
    | 29
    | 30
    | 31
    | 32
    | 33
    | 34
    | 35
    | 36
    | 37
    | 38
    | 39
    | 40
    | 41
    | 42
    | 43
    | 44
    | 45
    | 46
    | 47
    | 48
    | 49
    | 50
    | 51
    | 52
    | 53
    | 54
    | 55
    | 56
    | 57
    | 58
    | 59
    | 60
    | 61
    | 62
    | 63
    | 64
    | 65
    | 66
    | 67
    | 68
    | 69
    | 70
    | 71
    | 72
    | 73
    | 74
    | 75
    | 76
    | 77
    | 78 -> pos + 1 |> memarg refs |> instructions refs
    | 3 ->
        let c = get (pos + 1) in
        assert (c = 0);
        pos + 2 |> instructions refs
    | c -> failwith (Printf.sprintf "Bad instruction 0xFE 0x%02X" c)
  and opt_else refs pos =
    match get pos with
    | 0x05 -> pos + 1 |> instructions refs |> block_end |> instructions refs
    | _ -> pos |> block_end |> instructions refs
  and opt_catch refs pos =
    match get pos with
    | 0x07 -> pos + 1 |> tagidx refs |> instructions refs |> opt_catch refs
    | 0x05 -> pos + 1 |> instructions refs |> block_end |> instructions refs
    | _ -> pos |> block_end |> instructions refs
  and catch refs pos =
    match get pos with
    | 0 | 1 -> pos + 1 |> tagidx refs |> labelidx
    | 2 | 3 -> pos + 1 |> labelidx
    | c -> failwith (Printf.sprintf "bad catch 0x%02X" c)
  and block_end pos =
    match get pos with
    | 0x0B -> pos + 1
    | c -> failwith (Printf.sprintf "Bad instruction 0x%02X" c)
  in
  let locals refs pos = pos |> int |> valtype refs in
  let expr refs pos = pos |> instructions refs |> block_end in
  let func refs pos = pos |> vector (locals refs) |> expr refs in
  let elemkind pos =
    assert (get pos = 0);
    pos + 1
  in
  let elem refs pos =
    match get pos with
    | 0 -> pos + 1 |> expr refs |> vector (funcidx refs)
    | 1 -> pos + 1 |> elemkind |> vector (funcidx refs)
    | 2 -> pos + 1 |> tableidx refs |> expr refs |> elemkind |> vector (funcidx refs)
    | 3 -> pos + 1 |> elemkind |> vector (funcidx refs)
    | 4 -> pos + 1 |> expr refs |> vector (expr refs)
    | 5 -> pos + 1 |> reftype refs |> vector (expr refs)
    | 6 -> pos + 1 |> tableidx refs |> expr refs |> reftype refs |> vector (expr refs)
    | 7 -> pos + 1 |> reftype refs |> vector (expr refs)
    | c -> failwith (Printf.sprintf "Bad element 0x%02X" c)
  in
  func, elem, expr

(* Parse deps.json to extract dependency information *)

type dep_entry =
  { name : string
  ; root : bool
  ; export : string option
  ; import : (string * string) option
  ; reaches : string list
  }

let parse_dependencies ~dependencies primitives =
  (* Build the same augmented JSON that Binaryen.generate_dependencies creates *)
  let base_entries = Yojson.Basic.Util.to_list (Yojson.Basic.from_string dependencies) in
  let prim_entries =
    StringSet.fold
      (fun nm l ->
        `Assoc
          [ "name", `String ("js:" ^ nm); "import", `List [ `String "js"; `String nm ] ]
        :: l)
      primitives
      []
  in
  let all_entries = prim_entries @ base_entries in
  let find_field assoc key =
    List.find_map ~f:(fun (k, v) -> if String.equal k key then Some v else None) assoc
  in
  List.map
    ~f:(fun entry ->
      let assoc = Yojson.Basic.Util.to_assoc entry in
      let name =
        Yojson.Basic.Util.to_string
          (Option.value ~default:`Null (find_field assoc "name"))
      in
      let root =
        match find_field assoc "root" with
        | Some (`Bool b) -> b
        | _ -> false
      in
      let export =
        match find_field assoc "export" with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let import =
        match find_field assoc "import" with
        | Some (`List [ `String m; `String n ]) -> Some (m, n)
        | _ -> None
      in
      let reaches =
        match find_field assoc "reaches" with
        | Some (`List l) -> List.map ~f:Yojson.Basic.Util.to_string l
        | _ -> []
      in
      { name; root; export; import; reaches })
    all_entries

(* Main DCE function *)
let f ~dependencies ~opt_input_sourcemap ~input_file ~opt_output_sourcemap ~output_file =
  let contents = Fs.read_file input_file in
  let file_contents = Wasm_link.Read.open_in input_file contents in
  (* Parse type section *)
  let types = Wasm_link.Read.create_types () in
  Wasm_link.Read.type_section types file_contents;
  (* After type_section, type_mapping maps original indices to deduped indices.
     func_types and import descriptors already use deduped indices (via typeidx).
     The collector reads raw type indices from the binary, which are in original space.
     We remap them through type_mapping when processing refs.

     For a linked module, there should be no actual deduplication, so
     type_mapping should be identity and orig_type_count == type_count. *)
  let type_mapping = file_contents.type_mapping in
  let type_count = Wasm_link.Read.types_last_index types in
  let type_list = List.rev (Wasm_link.Read.types_rev_list types) in
  let subtyping_info = Array.concat type_list in

  (* Parse import section *)
  let intf = Wasm_link.Read.interface file_contents in
  let import_func_count = Array.length intf.imports.func in
  let import_table_count = Array.length intf.imports.table in
  let import_mem_count = Array.length intf.imports.mem in
  let import_global_count = Array.length intf.imports.global in
  let import_tag_count = Array.length intf.imports.tag in

  (* Parse function section *)
  let func_types = Wasm_link.Read.functions file_contents in
  let defined_func_count = Array.length func_types in
  let total_func_count = import_func_count + defined_func_count in

  (* Parse table section - just count *)
  let has_table_section = Wasm_link.Read.find_section file_contents 4 in
  let defined_table_count =
    if has_table_section then Wasm_link.Read.uint file_contents.ch else 0
  in
  let total_table_count = import_table_count + defined_table_count in

  (* Parse memory section *)
  let memories = Wasm_link.Read.memories file_contents in
  let defined_mem_count = Array.length memories in
  let total_mem_count = import_mem_count + defined_mem_count in

  (* Parse tag section *)
  let tags = Wasm_link.Read.tags file_contents in
  let defined_tag_count = Array.length tags in
  let total_tag_count = import_tag_count + defined_tag_count in

  (* Parse global section - count *)
  let has_global_section = Wasm_link.Read.find_section file_contents 6 in
  let defined_global_count =
    if has_global_section then Wasm_link.Read.uint file_contents.ch else 0
  in
  let total_global_count = import_global_count + defined_global_count in

  (* Parse export section *)
  let exports = intf.exports in

  (* Parse start function *)
  let start_func = Wasm_link.Read.start file_contents in

  (* Parse element section - count *)
  let has_elem_section = Wasm_link.Read.find_section file_contents 9 in
  let elem_count = if has_elem_section then Wasm_link.Read.uint file_contents.ch else 0 in

  (* Parse data count section *)
  let data_count = Wasm_link.Read.data_count file_contents in

  (* Build export index: name -> (kind, index) *)
  let export_by_name = String.Hashtbl.create 128 in
  Wasm_link.iter_exportable_info
    (fun kind lst ->
      List.iter
        ~f:(fun (name, idx) -> String.Hashtbl.replace export_by_name name (kind, idx))
        lst)
    exports;

  (* Parse dependencies *)
  let primitives = Linker.list_all () in
  let dep_entries = parse_dependencies ~dependencies primitives in

  (* Build dep entry lookup by name *)
  let dep_by_name = String.Hashtbl.create 128 in
  List.iter
    ~f:(fun entry -> String.Hashtbl.replace dep_by_name entry.name entry)
    dep_entries;

  (* Build dep entry lookup by import *)
  let dep_by_import = Poly.Hashtbl.create 128 in
  List.iter
    ~f:(fun entry ->
      match entry.import with
      | Some key -> Poly.Hashtbl.replace dep_by_import key entry
      | None -> ())
    dep_entries;

  (* Reachability sets *)
  let func_reachable = Array.make total_func_count false in
  let type_reachable = Array.make type_count false in
  let global_reachable = Array.make total_global_count false in
  let elem_reachable = Array.make elem_count false in
  let data_reachable = Array.make data_count false in
  let tag_reachable = Array.make total_tag_count false in

  (* Worklist *)
  let worklist_func = Queue.create () in
  let worklist_type = Queue.create () in
  let worklist_global = Queue.create () in
  let worklist_elem = Queue.create () in

  let mark_func idx =
    if idx >= 0 && idx < total_func_count && not func_reachable.(idx)
    then (
      func_reachable.(idx) <- true;
      Queue.push idx worklist_func)
  in
  let mark_type idx =
    if idx >= 0 && idx < type_count && not type_reachable.(idx)
    then (
      type_reachable.(idx) <- true;
      Queue.push idx worklist_type)
  in
  let mark_global idx =
    if idx >= 0 && idx < total_global_count && not global_reachable.(idx)
    then (
      global_reachable.(idx) <- true;
      Queue.push idx worklist_global)
  in
  let mark_elem idx =
    if idx >= 0 && idx < elem_count && not elem_reachable.(idx)
    then (
      elem_reachable.(idx) <- true;
      Queue.push idx worklist_elem)
  in
  let mark_data idx = if idx >= 0 && idx < data_count then data_reachable.(idx) <- true in
  let mark_tag idx =
    if idx >= 0 && idx < total_tag_count then tag_reachable.(idx) <- true
  in
  let process_refs r =
    List.iter ~f:mark_func r.funcs;
    (* Type indices from the collector are in original space;
       remap through type_mapping to deduped space.
       Negative indices are abstract heap types, not real type indices. *)
    List.iter ~f:(fun idx -> if idx >= 0 then mark_type type_mapping.(idx)) r.types;
    List.iter ~f:mark_global r.globals;
    List.iter ~f:mark_elem r.elems;
    List.iter ~f:mark_data r.datas;
    List.iter ~f:mark_tag r.tags
  in

  (* Resolve dep reaches: when a dep entry becomes reachable, mark its reaches targets *)
  let resolve_dep_reaches entry =
    List.iter
      ~f:(fun target_name ->
        match String.Hashtbl.find_opt dep_by_name target_name with
        | Some target_entry -> (
            match target_entry.export with
            | Some export_name -> (
                match String.Hashtbl.find_opt export_by_name export_name with
                | Some (Wasm_link.Func, idx) -> mark_func idx
                | Some (Wasm_link.Global, idx) -> mark_global idx
                | Some (Wasm_link.Tag, idx) -> mark_tag idx
                | Some ((Wasm_link.Table | Wasm_link.Mem), _) ->
                    () (* tables and memories are always kept *)
                | None -> ())
            | None -> ())
        | None -> ())
      entry.reaches
  in

  (* When an import becomes reachable, check dep entries *)
  let process_import_reachable kind (imp : Wasm_link.import) =
    ignore (kind : Wasm_link.exportable);
    match Poly.Hashtbl.find_opt dep_by_import (imp.module_, imp.name) with
    | Some entry -> resolve_dep_reaches entry
    | None -> ()
  in

  (* Seed roots *)

  (* 1. All exports are roots *)
  Wasm_link.iter_exportable_info
    (fun kind lst ->
      List.iter
        ~f:(fun (_name, idx) ->
          match kind with
          | Wasm_link.Func -> mark_func idx
          | Wasm_link.Global -> mark_global idx
          | Wasm_link.Tag -> mark_tag idx
          | Wasm_link.Table | Wasm_link.Mem ->
              () (* tables and memories are always kept *))
        lst)
    exports;

  (* 2. Start function is a root *)
  Option.iter ~f:mark_func start_func;

  (* 3. deps.json root entries *)
  List.iter ~f:(fun entry -> if entry.root then resolve_dep_reaches entry) dep_entries;

  (* Read code section positions for each defined function *)
  let has_code_section = Wasm_link.Read.find_section file_contents 10 in
  let func_code_offsets = Array.make defined_func_count (0, 0) in
  if has_code_section
  then (
    let n = Wasm_link.Read.uint file_contents.ch in
    assert (n = defined_func_count);
    for i = 0 to n - 1 do
      let body_start = Wasm_link.Read.pos_in file_contents.ch in
      let size = Wasm_link.Read.uint file_contents.ch in
      let code_start = Wasm_link.Read.pos_in file_contents.ch in
      func_code_offsets.(i) <- body_start, size + code_start - body_start;
      Wasm_link.Read.seek_in file_contents.ch (code_start + size)
    done);

  (* Read global section positions for each defined global *)
  let global_code_offsets = Array.make defined_global_count (0, 0) in
  if has_global_section
  then (
    ignore (Wasm_link.Read.find_section file_contents 6);
    let n = Wasm_link.Read.uint file_contents.ch in
    assert (n = defined_global_count);
    let _, _, scan_expr = collector contents in
    for i = 0 to n - 1 do
      let start_pos = Wasm_link.Read.pos_in file_contents.ch in
      (* Skip globaltype using the Read module *)
      Wasm_link.Read.seek_in file_contents.ch start_pos;
      ignore (Wasm_link.Read.globaltype file_contents file_contents.ch);
      let expr_start = Wasm_link.Read.pos_in file_contents.ch in
      (* Scan the init expr to find its end *)
      let r = create_refs () in
      let end_pos = scan_expr r expr_start in
      global_code_offsets.(i) <- start_pos, end_pos - start_pos;
      Wasm_link.Read.seek_in file_contents.ch end_pos
    done);

  (* Read element section data *)
  let elem_offsets = Array.make elem_count (0, 0) in
  if has_elem_section
  then (
    ignore (Wasm_link.Read.find_section file_contents 9);
    let n = Wasm_link.Read.uint file_contents.ch in
    assert (n = elem_count);
    let _, scan_elem, _ = collector contents in
    for i = 0 to n - 1 do
      let start_pos = Wasm_link.Read.pos_in file_contents.ch in
      let r = create_refs () in
      let end_pos = scan_elem r start_pos in
      elem_offsets.(i) <- start_pos, end_pos - start_pos;
      Wasm_link.Read.seek_in file_contents.ch end_pos
    done);

  (* Mark active element segments as roots (they have side effects) *)
  if has_elem_section
  then
    for i = 0 to elem_count - 1 do
      let start_pos, _size = elem_offsets.(i) in
      let kind = Char.code contents.[start_pos] in
      (* Active segments: kinds 0, 2, 4, 6 (even numbers) *)
      if kind land 1 = 0 then mark_elem i
    done;

  (* Parse data section for positions and active segment seeding *)
  let data_offsets = Array.make data_count (0, 0) in
  let has_data_section = Wasm_link.Read.find_section file_contents 11 in
  if has_data_section
  then (
    let n = Wasm_link.Read.uint file_contents.ch in
    assert (n = data_count);
    let _, _, scan_expr = collector contents in
    (* Read LEB128 uint and return (position after, value) *)
    let read_leb128 pos =
      let p = ref pos in
      let v = ref 0 in
      let shift = ref 0 in
      let b = ref (Char.code contents.[!p]) in
      while !b >= 128 do
        v := !v lor ((!b land 0x7f) lsl !shift);
        shift := !shift + 7;
        incr p;
        b := Char.code contents.[!p]
      done;
      v := !v lor (!b lsl !shift);
      incr p;
      !p, !v
    in
    (* Skip past a byte vector (LEB128 count + that many bytes) *)
    let skip_bytes pos =
      let pos, byte_count = read_leb128 pos in
      pos + byte_count
    in
    for i = 0 to n - 1 do
      let start_pos = Wasm_link.Read.pos_in file_contents.ch in
      let kind = Char.code contents.[start_pos] in
      let end_pos =
        match kind with
        | 0 ->
            (* Active, memory 0: seed as root, scan offset expr *)
            mark_data i;
            let r = create_refs () in
            let expr_end = scan_expr r (start_pos + 1) in
            process_refs r;
            skip_bytes expr_end
        | 1 ->
            (* Passive: not a root *)
            skip_bytes (start_pos + 1)
        | 2 ->
            (* Active, explicit memory: seed as root *)
            mark_data i;
            let memidx_end, _ = read_leb128 (start_pos + 1) in
            let r = create_refs () in
            let expr_end = scan_expr r memidx_end in
            process_refs r;
            skip_bytes expr_end
        | _ -> failwith (Printf.sprintf "Bad data segment kind %d" kind)
      in
      data_offsets.(i) <- (start_pos, end_pos - start_pos);
      Wasm_link.Read.seek_in file_contents.ch end_pos
    done);

  (* Worklist propagation *)

  (* Scan a defined function's code for references *)
  let scan_defined_func idx =
    let local_idx = idx - import_func_count in
    if local_idx >= 0 && local_idx < defined_func_count
    then (
      let scan_func, _, _ = collector contents in
      let r = create_refs () in
      let body_start, body_size = func_code_offsets.(local_idx) in
      if body_size > 0
      then (
        (* Skip the body size LEB *)
        let pos = ref body_start in
        while Char.code contents.[!pos] >= 128 do
          incr pos
        done;
        incr pos;
        ignore (scan_func r !pos));
      (* Also mark the function's type *)
      mark_type func_types.(local_idx);
      process_refs r)
  in

  (* Scan a defined global's init expr *)
  let scan_defined_global idx =
    let local_idx = idx - import_global_count in
    if local_idx >= 0 && local_idx < defined_global_count
    then
      let start_pos, size = global_code_offsets.(local_idx) in
      if size > 0
      then (
        (* Skip globaltype, scan init expr *)
        Wasm_link.Read.seek_in file_contents.ch start_pos;
        ignore (Wasm_link.Read.globaltype file_contents file_contents.ch);
        let expr_start = Wasm_link.Read.pos_in file_contents.ch in
        let _, _, scan_expr = collector contents in
        let r = create_refs () in
        ignore (scan_expr r expr_start);
        process_refs r)
  in

  (* Scan an element segment *)
  let scan_elem_segment idx =
    if idx >= 0 && idx < elem_count
    then (
      let _, scan_elem, _ = collector contents in
      let r = create_refs () in
      let start_pos, _size = elem_offsets.(idx) in
      ignore (scan_elem r start_pos);
      process_refs r)
  in

  (* Mark rec group members *)
  let rec_group_ranges =
    let arr = Array.make type_count 0 in
    let pos = ref 0 in
    List.iter
      ~f:(fun rectype ->
        let len = Array.length rectype in
        for i = 0 to len - 1 do
          arr.(!pos + i) <- !pos
        done;
        pos := !pos + len)
      type_list;
    arr
  in
  let rec_group_sizes =
    let arr = Array.make type_count 1 in
    let pos = ref 0 in
    List.iter
      ~f:(fun rectype ->
        let len = Array.length rectype in
        for i = 0 to len - 1 do
          arr.(!pos + i) <- len
        done;
        pos := !pos + len)
      type_list;
    arr
  in

  let process_type idx =
    (* Mark supertype *)
    (match subtyping_info.(idx).supertype with
    | Some s when s >= 0 -> mark_type s
    | _ -> ());
    (* Mark all members of the rec group *)
    let group_start = rec_group_ranges.(idx) in
    let group_size = rec_group_sizes.(idx) in
    for i = group_start to group_start + group_size - 1 do
      if not type_reachable.(i)
      then (
        type_reachable.(i) <- true;
        Queue.push i worklist_type)
    done;
    (* Scan type definition for type references *)
    let scan_comptype (ct : Wasm_link.comptype) =
      match ct with
      | Func { params; results } ->
          let mark_valtype_refs (v : Wasm_link.valtype) =
            match v with
            | Ref { typ = Type idx; _ } when idx >= 0 -> mark_type idx
            | _ -> ()
          in
          Array.iter ~f:mark_valtype_refs params;
          Array.iter ~f:mark_valtype_refs results
      | Struct fields ->
          Array.iter
            ~f:(fun (ft : Wasm_link.fieldtype) ->
              match ft.typ with
              | Val (Ref { typ = Type idx; _ }) when idx >= 0 -> mark_type idx
              | _ -> ())
            fields
      | Array ft -> (
          match ft.typ with
          | Val (Ref { typ = Type idx; _ }) when idx >= 0 -> mark_type idx
          | _ -> ())
    in
    scan_comptype subtyping_info.(idx).typ
  in

  (* Main worklist loop *)
  let continue = ref true in
  while !continue do
    continue := false;
    (* Process functions *)
    while not (Queue.is_empty worklist_func) do
      continue := true;
      let idx = Queue.pop worklist_func in
      if idx < import_func_count
      then (
        (* Import function: mark its type and check dep entries *)
        let imp = intf.imports.func.(idx) in
        (match (imp.desc : Wasm_link.importdesc) with
        | Func t -> mark_type t
        | _ -> ());
        process_import_reachable Wasm_link.Func imp)
      else scan_defined_func idx
    done;
    (* Process types *)
    while not (Queue.is_empty worklist_type) do
      continue := true;
      let idx = Queue.pop worklist_type in
      process_type idx
    done;
    (* Process globals *)
    while not (Queue.is_empty worklist_global) do
      continue := true;
      let idx = Queue.pop worklist_global in
      if idx < import_global_count
      then
        let imp = intf.imports.global.(idx) in
        process_import_reachable Wasm_link.Global imp
      else scan_defined_global idx
    done;
    (* Process elements *)
    while not (Queue.is_empty worklist_elem) do
      continue := true;
      let idx = Queue.pop worklist_elem in
      scan_elem_segment idx
    done;
    (* Check declarative element segments (kinds 3, 7).
       These must be kept if they declare any function that is reachable
       (for ref.func validation). When kept, all functions in the segment
       become reachable via scan_elem_segment on the next iteration. *)
    for i = 0 to elem_count - 1 do
      if not elem_reachable.(i)
      then
        let start_pos, _size = elem_offsets.(i) in
        let kind = Char.code contents.[start_pos] in
        if kind = 3 || kind = 7
        then (
          let _, scan_elem, _ = collector contents in
          let r = create_refs () in
          ignore (scan_elem r start_pos);
          let any_reachable =
            List.exists
              ~f:(fun idx ->
                idx >= 0 && idx < total_func_count && func_reachable.(idx))
              r.funcs
          in
          if any_reachable then mark_elem i)
    done
  done;

  (* Build mapping arrays: old index -> new dense index, -1 if removed *)
  let build_map reachable total import_count =
    let map = Array.make total (-1) in
    let new_import_count = ref 0 in
    for i = 0 to import_count - 1 do
      if reachable.(i)
      then (
        map.(i) <- !new_import_count;
        incr new_import_count)
    done;
    let new_defined_count = ref 0 in
    for i = import_count to total - 1 do
      if reachable.(i)
      then (
        map.(i) <- !new_import_count + !new_defined_count;
        incr new_defined_count)
    done;
    map, !new_import_count, !new_defined_count
  in

  let func_map, _new_import_func_count, new_defined_func_count =
    build_map func_reachable total_func_count import_func_count
  in
  (* Tables and memories are always kept — use identity maps *)
  let table_map = Array.init ~f:Fun.id total_table_count in
  let mem_map = Array.init ~f:Fun.id total_mem_count in
  let global_map, _new_import_global_count, new_defined_global_count =
    build_map global_reachable total_global_count import_global_count
  in
  let tag_map, _new_import_tag_count, _new_defined_tag_count =
    build_map tag_reachable total_tag_count import_tag_count
  in
  let elem_map, _, _ = build_map elem_reachable elem_count 0 in
  let data_map, _, _ = build_map data_reachable data_count 0 in

  (* Type mapping: we keep all types that belong to a rec group where at
     least one member is reachable *)
  let type_map = Array.make type_count (-1) in
  let new_type_count = ref 0 in
  let pos = ref 0 in
  let kept_rec_types = ref [] in
  List.iter
    ~f:(fun rectype ->
      let len = Array.length rectype in
      let group_reachable = ref false in
      for i = 0 to len - 1 do
        if type_reachable.(!pos + i) then group_reachable := true
      done;
      if !group_reachable
      then (
        kept_rec_types := rectype :: !kept_rec_types;
        for i = 0 to len - 1 do
          type_map.(!pos + i) <- !new_type_count + i
        done;
        new_type_count := !new_type_count + len);
      pos := !pos + len)
    type_list;
  let kept_rec_types = List.rev !kept_rec_types in

  (* Remap types within kept rec types *)
  let remap_heaptype (ht : Wasm_link.heaptype) : Wasm_link.heaptype =
    match ht with
    | Type idx ->
        (* Negative indices are intra-rec-group forward references
           (encoded via lnot); keep them as-is since they're relative *)
        if idx < 0
        then ht
        else
          let idx' = type_map.(idx) in
          assert (idx' >= 0);
          Type idx'
    | other -> other
  in
  let remap_reftype (rt : Wasm_link.reftype) : Wasm_link.reftype =
    { rt with typ = remap_heaptype rt.typ }
  in
  let remap_valtype (vt : Wasm_link.valtype) : Wasm_link.valtype =
    match vt with
    | Ref rt -> Ref (remap_reftype rt)
    | other -> other
  in
  let remap_storagetype (st : Wasm_link.storagetype) : Wasm_link.storagetype =
    match st with
    | Val vt -> Val (remap_valtype vt)
    | Packed _ -> st
  in
  let remap_fieldtype (ft : Wasm_link.fieldtype) : Wasm_link.fieldtype =
    { ft with typ = remap_storagetype ft.typ }
  in
  let remap_comptype (ct : Wasm_link.comptype) : Wasm_link.comptype =
    match ct with
    | Func { params; results } ->
        Func
          { params = Array.map ~f:remap_valtype params
          ; results = Array.map ~f:remap_valtype results
          }
    | Struct fields -> Struct (Array.map ~f:remap_fieldtype fields)
    | Array ft -> Array (remap_fieldtype ft)
  in
  let remap_subtype (st : Wasm_link.subtype) : Wasm_link.subtype =
    { final = st.final
    ; supertype = Option.map ~f:(fun i -> if i < 0 then i else type_map.(i)) st.supertype
    ; typ = remap_comptype st.typ
    }
  in
  let remapped_rec_types =
    List.map ~f:(fun rt -> Array.map ~f:remap_subtype rt) kept_rec_types
  in

  (* Index remapping for the scanner *)
  let maps =
    { Wasm_link.Scan.typ = type_map
    ; func = func_map
    ; table = table_map
    ; mem = mem_map
    ; global = global_map
    ; elem = elem_map
    ; data = data_map
    ; tag = tag_map
    }
  in

  (* Now write the output module *)
  let out_ch = open_out_bin output_file in
  output_string out_ch Wasm_link.Read.header;
  let buf = Buffer.create 100000 in

  (* Section 1: types *)
  let _st = Wasm_link.Write.types buf (Array.of_list remapped_rec_types) in
  Wasm_link.add_section out_ch ~id:1 buf;

  (* Section 2: imports *)
  let kept_imports = ref [] in
  let add_kept_imports kind imports =
    Array.iteri
      ~f:(fun i (imp : Wasm_link.import) ->
        let reachable =
          match kind with
          | Wasm_link.Func -> func_reachable.(i)
          | Wasm_link.Global -> global_reachable.(i)
          | Wasm_link.Tag -> tag_reachable.(i)
          | Wasm_link.Table | Wasm_link.Mem -> true
        in
        if reachable
        then
          (* Remap type indices in import desc *)
          let desc : Wasm_link.importdesc =
            match imp.desc with
            | Func t -> Func type_map.(t)
            | Table tt -> Table { tt with typ = remap_reftype tt.typ }
            | Mem l -> Mem l
            | Global gt -> Global { mut = gt.mut; typ = remap_valtype gt.typ }
            | Tag t -> Tag type_map.(t)
          in
          kept_imports := { imp with desc } :: !kept_imports)
      imports
  in
  add_kept_imports Wasm_link.Func intf.imports.func;
  add_kept_imports Wasm_link.Table intf.imports.table;
  add_kept_imports Wasm_link.Mem intf.imports.mem;
  add_kept_imports Wasm_link.Global intf.imports.global;
  add_kept_imports Wasm_link.Tag intf.imports.tag;
  let kept_imports_arr = Array.of_list (List.rev !kept_imports) in
  let wst = { Wasm_link.Write.type_index_count = !new_type_count } in
  Wasm_link.Write.imports wst buf kept_imports_arr;
  Wasm_link.add_section out_ch ~id:2 buf;

  (* Section 3: functions *)
  let new_func_types =
    Array.of_list
      (Array.to_list func_types
      |> List.mapi ~f:(fun i t -> i, t)
      |> List.filter_map ~f:(fun (i, t) ->
          if func_reachable.(import_func_count + i) then Some type_map.(t) else None))
  in
  Wasm_link.Write.functions buf new_func_types;
  Wasm_link.add_section out_ch ~id:3 buf;

  (* Section 4: tables — always kept, rewrite with scan *)
  if has_table_section && defined_table_count > 0
  then (
    ignore (Wasm_link.Read.find_section file_contents 4);
    let n = Wasm_link.Read.uint file_contents.ch in
    let positions = Wasm_link.Scan.create_position_data () in
    let section_start = Wasm_link.Read.pos_in file_contents.ch in
    Wasm_link.Scan.table_section positions maps buf contents ~count:n section_start;
    Wasm_link.add_section out_ch ~id:4 ~count:n buf);

  (* Section 5: memories — always kept *)
  if Array.length memories > 0
  then (
    Wasm_link.Write.memories buf memories;
    Wasm_link.add_section out_ch ~id:5 buf);

  (* Section 13: tags *)
  let kept_tags =
    Array.of_list
      (Array.to_list tags
      |> List.mapi ~f:(fun i t -> i, t)
      |> List.filter_map ~f:(fun (i, t) ->
          if tag_reachable.(import_tag_count + i) then Some type_map.(t) else None))
  in
  if Array.length kept_tags > 0
  then (
    Wasm_link.Write.tags buf kept_tags;
    Wasm_link.add_section out_ch ~id:13 buf);

  (* Section 6: globals - rewrite with scan *)
  if has_global_section && new_defined_global_count > 0
  then (
    ignore (Wasm_link.Read.find_section file_contents 6);
    let n = Wasm_link.Read.uint file_contents.ch in
    let positions = Wasm_link.Scan.create_position_data () in
    if new_defined_global_count = defined_global_count
    then (
      Wasm_link.Scan.global_section
        positions
        maps
        buf
        contents
        ~count:n
        (Wasm_link.Read.pos_in file_contents.ch);
      Wasm_link.add_section out_ch ~id:6 ~count:n buf)
    else
      (* Need to selectively copy globals. For each global, scan and buffer individually *)
      let section_buf = Buffer.create 10000 in
      let count = ref 0 in
      for i = 0 to n - 1 do
        let start_pos, _size = global_code_offsets.(i) in
        if global_reachable.(import_global_count + i)
        then (
          incr count;
          Wasm_link.Scan.clear_position_data positions;
          Wasm_link.Scan.global_section positions maps buf contents ~count:1 start_pos;
          Buffer.add_buffer section_buf buf;
          Buffer.clear buf)
      done;
      Buffer.add_buffer buf section_buf;
      Wasm_link.add_section out_ch ~id:6 ~count:!count buf);

  (* Section 7: exports - remap indices *)
  let export_buf = Buffer.create 1000 in
  let export_count = ref 0 in
  Wasm_link.iter_exportable_info
    (fun kind lst ->
      List.iter
        ~f:(fun (name, idx) ->
          let reachable =
            match kind with
            | Wasm_link.Func -> func_reachable.(idx)
            | Wasm_link.Global -> global_reachable.(idx)
            | Wasm_link.Tag -> tag_reachable.(idx)
            | Wasm_link.Table | Wasm_link.Mem -> true
          in
          if reachable
          then (
            let map =
              match kind with
              | Wasm_link.Func -> func_map
              | Wasm_link.Table -> table_map
              | Wasm_link.Mem -> mem_map
              | Wasm_link.Global -> global_map
              | Wasm_link.Tag -> tag_map
            in
            incr export_count;
            Wasm_link.Write.export export_buf kind name map.(idx)))
        lst)
    exports;
  Wasm_link.Write.uint buf !export_count;
  Buffer.add_buffer buf export_buf;
  Wasm_link.add_section out_ch ~id:7 buf;

  (* Section 8: start *)
  (match start_func with
  | Some idx ->
      let new_idx = func_map.(idx) in
      assert (new_idx >= 0);
      Wasm_link.Write.start buf new_idx;
      Wasm_link.add_section out_ch ~id:8 buf
  | None -> ());

  (* Section 9: elements - rewrite with scan *)
  if has_elem_section && elem_count > 0
  then (
    let kept_count = ref 0 in
    for i = 0 to elem_count - 1 do
      if elem_reachable.(i)
      then (
        incr kept_count;
        let start_pos, _size = elem_offsets.(i) in
        Wasm_link.Scan.elem_section maps buf contents ~count:1 start_pos)
    done;
    Wasm_link.add_section out_ch ~id:9 ~count:!kept_count buf);

  (* Section 12: data count *)
  let kept_data_count =
    let c = ref 0 in
    for i = 0 to data_count - 1 do
      if data_reachable.(i) then incr c
    done;
    !c
  in
  if kept_data_count > 0
  then (
    Wasm_link.Write.data_count buf kept_data_count;
    Wasm_link.add_section out_ch ~id:12 buf);

  (* Section 10: code - rewrite function bodies *)
  let code_pieces = Buffer.create 100000 in
  let resize_data = Wasm_link.Scan.create_resize_data () in
  Wasm_link.Write.uint code_pieces new_defined_func_count;
  let scan_func = Wasm_link.Scan.func resize_data maps buf contents in
  for i = 0 to defined_func_count - 1 do
    if func_reachable.(import_func_count + i)
    then (
      let body_start, _total_size = func_code_offsets.(i) in
      Wasm_link.Read.seek_in file_contents.ch body_start;
      let _size = Wasm_link.Read.uint file_contents.ch in
      let code_start = Wasm_link.Read.pos_in file_contents.ch in
      Wasm_link.Scan.clear_resize_data resize_data;
      Wasm_link.Scan.push_resize resize_data code_start 0;
      scan_func code_start;
      let p = Buffer.length code_pieces in
      Wasm_link.Write.uint code_pieces (Buffer.length buf);
      let p' = Buffer.length code_pieces in
      let delta = p' - p - code_start + body_start in
      resize_data.delta.(0) <- delta;
      Buffer.add_buffer code_pieces buf;
      Buffer.clear buf)
  done;
  Wasm_link.add_section out_ch ~id:10 code_pieces;
  (* For MVP, if source map requested, copy it with adjustment.
     Full source map support would need careful offset tracking. *)
  (match opt_input_sourcemap, opt_output_sourcemap with
  | Some input_sm, Some output_sm ->
      (* Simple copy for now - the source map won't be perfectly accurate
         after DCE but will be usable *)
      let sm_contents = Fs.read_file input_sm in
      Fs.write_file ~name:output_sm ~contents:sm_contents
  | _ -> ());

  (* Section 11: data — selectively write reachable segments *)
  if has_data_section && kept_data_count > 0
  then (
    let section_buf = Buffer.create 10000 in
    for i = 0 to data_count - 1 do
      if data_reachable.(i)
      then (
        let start_pos, _size = data_offsets.(i) in
        Wasm_link.Scan.data_section maps buf contents ~count:1 start_pos;
        Buffer.add_buffer section_buf buf;
        Buffer.clear buf)
    done;
    Buffer.add_buffer buf section_buf;
    Wasm_link.add_section out_ch ~id:11 ~count:kept_data_count buf);

  (* Custom section: name *)
  let name_section = Wasm_link.Read.focus_on_custom_section file_contents "name" in
  let has_name_section =
    match Wasm_link.Read.get_custom_section file_contents "name" with
    | Some _ -> true
    | None -> false
  in
  if has_name_section
  then (
    let name_section_buffer = Buffer.create 10000 in
    Wasm_link.Write.name name_section_buffer "name";
    (* 1: function names *)
    if Wasm_link.Read.find_section name_section 1
    then (
      let n = Wasm_link.Read.uint name_section.ch in
      let count = ref 0 in
      for _ = 1 to n do
        let idx = Wasm_link.Read.uint name_section.ch in
        let nm = Wasm_link.Read.name name_section.ch in
        if idx < total_func_count && func_reachable.(idx)
        then (
          incr count;
          Wasm_link.Write.nameassoc buf func_map.(idx) nm)
      done;
      Wasm_link.add_subsection name_section_buffer ~id:1 ~count:!count buf);
    (* Skip other name subsections for MVP - they can be added later *)
    Wasm_link.add_section out_ch ~id:0 name_section_buffer);

  close_out out_ch;

  (* Return surviving JS primitives: imports from module "js" that are still reachable *)
  let surviving = ref StringSet.empty in
  Array.iteri
    ~f:(fun i (imp : Wasm_link.import) ->
      if func_reachable.(i) && String.equal imp.module_ "js"
      then surviving := StringSet.add imp.name !surviving)
    intf.imports.func;
  !surviving
