;; Wasm_of_ocaml runtime support
;; http://www.ocsigen.org/js_of_ocaml/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, with linking exception;
;; either version 2.1 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(module
   (import "stdlib" "caml_global_data"
      (global $caml_global_data (mut (ref $block))))
   (import "obj" "caml_callback_2"
      (func $caml_callback_2
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "jslib" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "bindings" "get_named_global"
      (func $get_named_global (param anyref) (result anyref)))
   (import "bindings" "get_ocaml_unit_list"
      (func $get_ocaml_unit_list (result anyref)))
   (import "bindings" "get_prim_list"
      (func $get_prim_list (result anyref)))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))

   (func (export "caml_terminfo_rows")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   ;; Grow caml_global_data to accommodate at least len+1 entries.
   ;; caml_global_data is a $block: index 0 is the tag, data starts at 1,
   ;; so the array length must be len + 2.
   (func (export "caml_realloc_global")
      (param $len (ref eq)) (result (ref eq))
      (local $new_len i32)
      (local $old (ref $block))
      (local $new (ref $block))
      (local.set $new_len
         (i32.add
            (i31.get_u (ref.cast (ref i31) (local.get $len)))
            (i32.const 2)))
      (if (i32.gt_u (local.get $new_len)
                     (array.len (global.get $caml_global_data)))
         (then
            (local.set $old (global.get $caml_global_data))
            (local.set $new
               (array.new $block (ref.i31 (i32.const 0)) (local.get $new_len)))
            (array.copy $block $block
               (local.get $new) (i32.const 0)
               (local.get $old) (i32.const 0)
               (array.len (local.get $old)))
            (global.set $caml_global_data (local.get $new))))
      (ref.i31 (i32.const 0)))

   ;; Bytecode sections: initialized to empty record so that
   ;; Symtable.init_toplevel() (called from Toploop module init) does not
   ;; crash if it runs before the dynlink library populates the real data.
   ;; Layout: { symb: GlobalMap.t; crcs: list; prim: list; dlpt: list }
   ;; GlobalMap.t = { cnt: int; tbl: Map.t } where Map.empty = 0
   (global $bytecode_sections (mut (ref eq))
      (array.new_fixed $block 5
         (ref.i31 (i32.const 0))
         (array.new_fixed $block 3
            (ref.i31 (i32.const 0))
            (ref.i31 (i32.const 0))
            (ref.i31 (i32.const 0)))
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))))

   (func (export "wasm_dynlink_init_sections")
      (param $sections (ref eq)) (result (ref eq))
      (global.set $bytecode_sections (local.get $sections))
      (ref.i31 (i32.const 0)))

   ;; Compile callback: set by OCaml-side init code
   (global $toplevel_compile (mut (ref eq)) (ref.i31 (i32.const 0)))

   (func (export "wasm_toplevel_init_compile")
      (param $f (ref eq)) (result (ref eq))
      (global.set $toplevel_compile (local.get $f))
      (ref.i31 (i32.const 0)))

   ;; Called by OCaml's Dynlink/Toploop to compile bytecode.
   ;; Passes the raw code value and debug info to the registered callback,
   ;; which compiles to Wasm and returns a (unit -> unit) thunk.
   ;; Returns (0, thunk) as expected by the caller.
   (func (export "caml_reify_bytecode")
      (param $code (ref eq)) (param $debug (ref eq)) (param $digest (ref eq))
      (result (ref eq))
      (if (ref.test (ref i31) (global.get $toplevel_compile))
         (then
            (call $caml_failwith
               (@string "Toplevel compile callback not initialized"))
            (unreachable)))
      (array.new_fixed $block 3
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))
         (call $caml_callback_2
            (global.get $toplevel_compile)
            (local.get $code)
            (local.get $debug))))

   (func (export "caml_static_release_bytecode")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_get_section_table")
      (param (ref eq)) (result (ref eq))
      (call $caml_failwith (@string "caml_get_section_table: not implemented"))
      (unreachable))

   ;; Returns the bytecode_sections record registered by OCaml init code.
   ;; { symb: GlobalMap.t; crcs: ...; prim: string list; dlpt: string list }
   (func (export "caml_dynlink_get_bytecode_sections")
      (param (ref eq)) (result (ref eq))
      (global.get $bytecode_sections))

   ;; Stubs for primitives required by compiler-libs.toplevel.
   ;; These must be proper Wasm exports so that any failure raises an OCaml
   ;; exception (via caml_failwith) instead of a JS Error from dummy stubs,
   ;; which would cause "illegal cast" in OCaml exception handlers.

   (func (export "caml_sys_modify_argv")
      (param (ref eq)) (result (ref eq))
      ;; no-op: Sys.argv modification is not meaningful in Wasm
      (ref.i31 (i32.const 0)))

   (func (export "caml_get_current_environment")
      (param (ref eq)) (result (ref eq))
      (call $caml_failwith
         (@string "caml_get_current_environment: not available in Wasm"))
      (unreachable))

   (func (export "caml_invoke_traced_function")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $caml_failwith
         (@string "caml_invoke_traced_function: not available in Wasm"))
      (unreachable))

   ;; Look up a named Wasm global in imports.OCaml and return its value.
   ;; Returns 0 (as i31ref) if not found.
   (func (export "wasm_get_named_global")
      (param $name (ref eq)) (result (ref eq))
      (local $result anyref)
      (local.set $result
         (call $get_named_global
            (call $unwrap (call $caml_jsstring_of_string (local.get $name)))))
      (if (ref.is_null (local.get $result))
         (then (return (ref.i31 (i32.const 0)))))
      (call $wrap (local.get $result)))

   ;; Return a '\x00'-separated string of all named Wasm global names.
   (func (export "wasm_get_ocaml_unit_list")
      (param (ref eq)) (result (ref eq))
      (call $caml_string_of_jsstring
         (call $wrap (call $get_ocaml_unit_list))))

   ;; Return a '\x00'-separated string of all available primitive names.
   (func (export "wasm_get_prim_list")
      (param (ref eq)) (result (ref eq))
      (call $caml_string_of_jsstring
         (call $wrap (call $get_prim_list))))
)
