(* Js_of_ocaml
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

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Wasm_of_ocaml_compiler

(* A module with four functions of type [i32 -> i32]. Only [keep] (index 1)
   is exported; it calls [keep2] (index 3). The two other functions are
   dead. Each instruction is given a source location: the line number
   tells which function it belongs to (1x: dead1, 2x: keep, 3x: dead2,
   4x: keep2). *)
let functions =
  [ (* dead1 *)
    [ 10, "\x20\x00" (* local.get 0 *)
    ; 11, "\x41\x01" (* i32.const 1 *)
    ; 12, "\x6a" (* i32.add *)
    ]
  ; (* keep *)
    [ 20, "\x20\x00" (* local.get 0 *)
    ; 21, "\x10\x03" (* call 3 *)
    ; 22, "\x41\x03" (* i32.const 3 *)
    ; 23, "\x6c" (* i32.mul *)
    ]
  ; (* dead2 *)
    [ 30, "\x20\x00" (* local.get 0 *)
    ; 31, "\x10\x00" (* call 0 *)
    ; 32, "\x41\x05" (* i32.const 5 *)
    ; 33, "\x6b" (* i32.sub *)
    ]
  ; (* keep2 *)
    [ 40, "\x20\x00" (* local.get 0 *)
    ; 41, "\x41\x07" (* i32.const 7 *)
    ; 42, "\x73" (* i32.xor *)
    ]
  ]

(* Sizes are all below 128, so they fit in one byte *)
let section b id contents =
  Buffer.add_char b (Char.chr id);
  Buffer.add_char b (Char.chr (String.length contents));
  Buffer.add_string b contents

(* Returns the module, the source map positions (offset, line) and, for
   each function, the offset following its [end] instruction *)
let build_module () =
  let b = Buffer.create 100 in
  Buffer.add_string b "\x00asm\x01\x00\x00\x00";
  section b 1 "\x01\x60\x01\x7f\x01\x7f";
  section b 3 "\x04\x00\x00\x00\x00";
  section b 7 "\x01\x04keep\x00\x01";
  let code = Buffer.create 100 in
  Buffer.add_char code '\x04';
  let positions = ref [] in
  let ends = ref [] in
  List.iter
    ~f:(fun instrs ->
      let body = Buffer.create 20 in
      Buffer.add_char body '\x00' (* no local *);
      let rel =
        List.map
          ~f:(fun (line, bytes) ->
            let pos = Buffer.length body in
            Buffer.add_string body bytes;
            pos, line)
          instrs
      in
      Buffer.add_char body '\x0b' (* end *);
      Buffer.add_char code (Char.chr (Buffer.length body));
      let start = Buffer.length code in
      Buffer.add_buffer code body;
      positions := List.map ~f:(fun (pos, line) -> start + pos, line) rel @ !positions;
      ends := Buffer.length code :: !ends)
    functions;
  (* The code section contents start after its id and size bytes *)
  let offset = Buffer.length b + 2 in
  section b 10 (Buffer.contents code);
  ( Buffer.contents b
  , List.rev_map ~f:(fun (pos, line) -> offset + pos, line) !positions
  , List.rev_map ~f:(fun pos -> offset + pos) !ends )

let print_mappings code (sm : Source_map.Standard.t) =
  List.iter
    ~f:(fun (m : Source_map.map) ->
      match m with
      | Gen_Ori { gen_col; ori_line; _ } ->
          Printf.printf
            "line %d -> %d (opcode 0x%02x)\n"
            ori_line
            gen_col
            (Char.code code.[gen_col])
      | Gen { gen_col; _ } ->
          if gen_col < String.length code
          then
            Printf.printf
              "none -> %d (opcode 0x%02x)\n"
              gen_col
              (Char.code code.[gen_col])
          else Printf.printf "none -> %d (end of module)\n" gen_col
      | Gen_Ori_Name _ -> assert false)
    (Source_map.Mappings.decode_exn sm.mappings)

(* Link the module, keeping only [keep], and return the linked module
   and its source map *)
let link code sm =
  let output_file = Filename.temp_file "linked" ".wasm" in
  let { Wasm_link.source_map; _ } =
    Wasm_link.f
      ~dependencies:
        [ { name = "root"
          ; export = None
          ; import = None
          ; reaches = [ "keep" ]
          ; root = true
          }
        ; { name = "keep"
          ; export = Some "keep"
          ; import = None
          ; reaches = []
          ; root = false
          }
        ]
      [ { module_name = "OCaml"
        ; file = "m.wasm"
        ; code = Some code
        ; opt_source_map = Some sm
        }
      ]
      ~output_file
  in
  let code = Fs.read_file output_file in
  Sys.remove output_file;
  code, Source_map.to_standard source_map

let%expect_test "dead code elimination drops the mappings of removed functions" =
  let code, positions, _ = build_module () in
  let sm =
    { (Source_map.Standard.empty ~inline_source_content:false) with
      sources = [ "a.ml" ]
    ; mappings =
        Source_map.Mappings.encode
          (List.map
             ~f:(fun (gen_col, ori_line) : Source_map.map ->
               Gen_Ori { gen_line = 1; gen_col; ori_source = 0; ori_line; ori_col = 0 })
             positions)
    }
  in
  print_mappings code sm;
  [%expect
    {|
    line 10 -> 38 (opcode 0x20)
    line 11 -> 40 (opcode 0x41)
    line 12 -> 42 (opcode 0x6a)
    line 20 -> 46 (opcode 0x20)
    line 21 -> 48 (opcode 0x10)
    line 22 -> 50 (opcode 0x41)
    line 23 -> 52 (opcode 0x6c)
    line 30 -> 56 (opcode 0x20)
    line 31 -> 58 (opcode 0x10)
    line 32 -> 60 (opcode 0x41)
    line 33 -> 62 (opcode 0x6b)
    line 40 -> 66 (opcode 0x20)
    line 41 -> 68 (opcode 0x41)
    line 42 -> 70 (opcode 0x73)
    |}];
  let code, sm = link code sm in
  print_mappings code sm;
  (* Only the mappings of [keep] and [keep2] remain, still pointing at
     the same instructions *)
  [%expect
    {|
    line 20 -> 36 (opcode 0x20)
    line 21 -> 38 (opcode 0x10)
    line 22 -> 40 (opcode 0x41)
    line 23 -> 42 (opcode 0x6c)
    none -> 44 (opcode 0x07)
    line 40 -> 46 (opcode 0x20)
    line 41 -> 48 (opcode 0x41)
    line 42 -> 50 (opcode 0x73)
    |}]

let%expect_test "the end of a function stays unmapped when the next function is removed" =
  let code, positions, ends = build_module () in
  (* As generated by [Wasm_output]: a mapping without origin follows the
     [end] instruction of each function. [dead2] has no mappings. *)
  let sm =
    { (Source_map.Standard.empty ~inline_source_content:false) with
      sources = [ "a.ml" ]
    ; mappings =
        Source_map.Mappings.encode
          (List.sort
             ~cmp:(fun (m : Source_map.map) (m' : Source_map.map) ->
               let gen_col (m : Source_map.map) =
                 match m with
                 | Gen { gen_col; _ }
                 | Gen_Ori { gen_col; _ }
                 | Gen_Ori_Name { gen_col; _ } -> gen_col
               in
               compare (gen_col m) (gen_col m'))
             (List.filter_map
                ~f:(fun (gen_col, ori_line) : Source_map.map option ->
                  if ori_line / 10 = 3
                  then None
                  else
                    Some
                      (Gen_Ori
                         { gen_line = 1; gen_col; ori_source = 0; ori_line; ori_col = 0 }))
                positions
             @ List.filteri
                 ~f:(fun i _ -> i <> 2)
                 (List.map
                    ~f:(fun gen_col : Source_map.map -> Gen { gen_line = 1; gen_col })
                    ends)))
    }
  in
  let code, sm = link code sm in
  print_mappings code sm;
  (* The function [keep] is followed by [keep2]: its last location does
     not extend over [keep2] *)
  [%expect
    {|
    none -> 34 (opcode 0x09)
    line 20 -> 36 (opcode 0x20)
    line 21 -> 38 (opcode 0x10)
    line 22 -> 40 (opcode 0x41)
    line 23 -> 42 (opcode 0x6c)
    none -> 44 (opcode 0x07)
    line 40 -> 46 (opcode 0x20)
    line 41 -> 48 (opcode 0x41)
    line 42 -> 50 (opcode 0x73)
    none -> 52 (opcode 0x00)
    |}]

let%expect_test "a dependency graph referring to an unknown node is rejected" =
  let code, _, _ = build_module () in
  let output_file = Filename.temp_file "linked" ".wasm" in
  (try
     ignore
       (Wasm_link.f
          ~dependencies:
            [ { name = "root"
              ; export = None
              ; import = None
              ; reaches = [ "kep" ]
              ; root = true
              }
            ; { name = "keep"
              ; export = Some "keep"
              ; import = None
              ; reaches = []
              ; root = false
              }
            ]
          [ { module_name = "OCaml"
            ; file = "m.wasm"
            ; code = Some code
            ; opt_source_map = None
            }
          ]
          ~output_file
         : Wasm_link.output)
   with Failure msg -> print_endline msg);
  [%expect {| In the dependency graph, node root reaches an unknown node kep. |}]

(* Tests on modules written in binary. A module is given as a list of
   sections (id, contents). *)

let module_ sections =
  let b = Buffer.create 100 in
  Buffer.add_string b "\x00asm\x01\x00\x00\x00";
  List.iter ~f:(fun (id, contents) -> section b id contents) sections;
  Buffer.contents b

let keep_dependencies : Wasm_link.dependency list =
  [ { name = "root"; export = None; import = None; reaches = [ "keep" ]; root = true }
  ; { name = "keep"; export = Some "keep"; import = None; reaches = []; root = false }
  ]

(* Link modules given as triples (module name, file name, contents). On
   failure, print the error message and whether an output file remains. *)
let link_modules ?dependencies ?(names = false) inputs =
  let output_file = Filename.temp_file "linked" ".wasm" in
  Sys.remove output_file;
  match
    Wasm_link.f
      ?dependencies
      ~names
      (List.map
         ~f:(fun (module_name, file, code) ->
           { Wasm_link.module_name; file; code = Some code; opt_source_map = None })
         inputs)
      ~output_file
  with
  | (_ : Wasm_link.output) ->
      let code = Fs.read_file output_file in
      Sys.remove output_file;
      Some code
  | exception Failure msg ->
      print_endline msg;
      if Sys.file_exists output_file then print_endline "The output file remains.";
      None

(* Print the given sections of a module, in hexadecimal; the custom
   sections are printed as strings *)
let print_sections ids code =
  let pos = ref 8 in
  let rec uint () =
    let c = Char.code code.[!pos] in
    incr pos;
    if c < 128 then c else c - 128 + (uint () lsl 7)
  in
  while !pos < String.length code do
    let id = uint () in
    let size = uint () in
    let contents = String.sub code ~pos:!pos ~len:size in
    pos := !pos + size;
    if List.mem ~eq:Int.equal id ids
    then
      if id = 0
      then Printf.printf "%2d: %S\n" id contents
      else (
        Printf.printf "%2d:" id;
        String.iter ~f:(fun c -> Printf.printf " %02x" (Char.code c)) contents;
        print_newline ())
  done

let%expect_test "custom descriptors" =
  (* A function type, so that the type indices of the next module change *)
  let other =
    module_ [ 1, "\x01\x60\x00\x01\x7e"; 3, "\x01\x00"; 10, "\x01\x04\x00\x42\x00\x0b" ]
  in
  (* (rec (type $a (descriptor $b) (struct)) (type $b (describes $a) (struct)))
     (type $f (func (param (ref null (exact $a)))))
     (import "m" "f" (func (exact (type $f))))
     (func (type $f)
       (drop (ref.get_desc $a (local.get 0))) (drop (ref.null (exact $a)))) *)
  let code =
    module_
      [ 1, "\x02\x4e\x02\x4d\x01\x5f\x00\x4c\x00\x5f\x00\x60\x01\x63\x62\x00\x00"
      ; 2, "\x01\x01m\x01f\x20\x02"
      ; 3, "\x01\x02"
      ; 10, "\x01\x0c\x00\x20\x00\xfb\x22\x00\x1a\xd0\x62\x00\x1a\x0b"
      ]
  in
  let linked = link_modules [ "a", "a.wasm", other; "b", "b.wasm", code ] in
  Option.iter ~f:(print_sections [ 1; 2; 3; 10 ]) linked;
  [%expect
    {|
            1: 03 60 00 01 7e 4e 02 4d 02 5f 00 4c 01 5f 00 60 01 63 62 01 00
            2: 01 01 6d 01 66 20 03
            3: 02 00 03
           10: 02 04 00 42 00 0b 0c 00 20 00 fb 22 01 1a d0 62 01 1a 0b
           |}];
  (* The linker can read back its output *)
  Option.iter
    ~f:(fun linked ->
      Option.iter
        ~f:(fun linked' -> Printf.printf "%b\n" (String.equal linked linked'))
        (link_modules [ "a", "linked.wasm", linked ]))
    linked;
  [%expect {| true |}]

let%expect_test "an import resolving to several exports is rejected" =
  let export =
    module_
      [ 1, "\x01\x60\x00\x00"
      ; 3, "\x01\x00"
      ; 7, "\x01\x01f\x00\x00"
      ; 10, "\x01\x02\x00\x0b"
      ]
  in
  let import = module_ [ 1, "\x01\x60\x00\x00"; 2, "\x01\x01m\x01f\x00\x00" ] in
  ignore
    (link_modules
       [ "m", "m1.wasm", export; "m", "m2.wasm", export; "a", "a.wasm", import ]);
  [%expect
    {| In module a.wasm, the import m / f is ambiguous: it is exported by both m1.wasm and m2.wasm |}];
  (* This is fine when the export is not used to resolve an import *)
  ignore
    (link_modules ~dependencies:[] [ "m", "m1.wasm", export; "m", "m2.wasm", export ]);
  [%expect {||}]

let%expect_test "malformed modules are reported" =
  let code, _, _ = build_module () in
  let test code = ignore (link_modules [ "a", "a.wasm", code ]) in
  (* Truncated file *)
  test (String.sub code ~pos:0 ~len:(String.length code - 3));
  [%expect
    {| a.wasm: malformed Wasm binary (section extending past the end at offset 35) |}];
  (* Unknown instruction *)
  test (module_ [ 1, "\x01\x60\x00\x00"; 3, "\x01\x00"; 10, "\x01\x03\x00\x27\x0b" ]);
  [%expect {| a.wasm: malformed Wasm binary (bad instruction 0x27 at offset 23) |}];
  (* Call to a function which does not exist *)
  test (module_ [ 1, "\x01\x60\x00\x00"; 3, "\x01\x00"; 10, "\x01\x04\x00\x10\x05\x0b" ]);
  [%expect {| a.wasm: malformed Wasm binary (truncated input or index out of range) |}];
  (* Bad import kind *)
  test (module_ [ 1, "\x01\x60\x00\x00"; 2, "\x01\x01m\x01f\x07\x00" ]);
  [%expect {| a.wasm: malformed Wasm binary (unknown import kind 0x07 at offset 22) |}]
