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

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))

   (func (export "caml_terminfo_rows")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   ;; Grow caml_global_data to accommodate at least len+1 entries
   (func (export "caml_realloc_global")
      (param $len (ref eq)) (result (ref eq))
      (local $new_len i32)
      (local $old (ref $block))
      (local $new (ref $block))
      (local.set $new_len
         (i32.add
            (i31.get_u (ref.cast (ref i31) (local.get $len)))
            (i32.const 1)))
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

   ;; Bytecode sections: set by OCaml-side init code
   (global $bytecode_sections (mut (ref eq)) (ref.i31 (i32.const 0)))

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
)
