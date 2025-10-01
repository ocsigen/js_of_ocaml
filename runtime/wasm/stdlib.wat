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
   (import "hash" "caml_string_hash"
      (func $caml_string_hash
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "string" "caml_string_equal"
      (func $caml_string_equal
         (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "obj" "caml_callback_1"
      (func $caml_callback_1
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "obj" "caml_callback_2"
      (func $caml_callback_2
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "string" "caml_string_concat"
      (func $caml_string_concat
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "printexc" "caml_format_exception"
      (func $caml_format_exception (param (ref eq)) (result (ref eq))))
   (import "sys" "ocaml_exit" (tag $ocaml_exit))
   (import "fail" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "fail" "javascript_exception"
      (tag $javascript_exception (param externref)))
   (import "bindings" "write" (func $write (param i32) (param anyref)))
   (import "bindings" "exit" (func $exit (param i32)))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))

   (type $assoc
      (struct
         (field (ref $bytes))
         (field (mut (ref eq)))
         (field (mut (ref null $assoc)))))

   (type $assoc_array (array (mut (ref null $assoc))))

   (global $Named_value_size i32 (i32.const 13))

   (global $named_value_table (ref $assoc_array)
     (array.new $assoc_array (ref.null $assoc) (global.get $Named_value_size)))

   (global $symbol_table (mut (ref $assoc_array))
      (array.new $assoc_array (ref.null $assoc) (i32.const 1)))

   (@string $predef_prefix "predef:")

   ;; Build a symbol key: for predefs, prepend "predef:" to the name.
   ;; For compunits (is_predef=0), return name unchanged.
   (func $make_symbol_key (param $is_predef i32) (param $name (ref $bytes))
      (result (ref $bytes))
      (local $len i32)
      (local $key (ref $bytes))
      (if (i32.eqz (local.get $is_predef))
         (then (return (local.get $name))))
      (local.set $len (array.len (local.get $name)))
      (local.set $key
         (array.new $bytes (i32.const 0) (i32.add (local.get $len) (i32.const 7))))
      (array.copy $bytes $bytes
         (local.get $key) (i32.const 0)
         (ref.cast (ref $bytes) (global.get $predef_prefix)) (i32.const 0) (i32.const 7))
      (array.copy $bytes $bytes
         (local.get $key) (i32.const 7)
         (local.get $name) (i32.const 0)
         (local.get $len))
      (local.get $key))
   (global $symbol_table_size (mut i32) (i32.const 1))

   (func $assoc_find
      (param $s (ref eq)) (param $l (ref null $assoc)) (result (ref null $assoc))
      (local $a (ref $assoc))
      (block $tail (result (ref null $assoc))
         (loop $loop
            (local.set $a
               (br_on_cast_fail $tail (ref null eq) (ref $assoc) (local.get $l)))
            (if (i31.get_u
                   (ref.cast (ref i31)
                       (call $caml_string_equal
                          (local.get $s)
                          (struct.get $assoc 0 (local.get $a)))))
               (then
                  (return (local.get $a))))
            (local.set $l (struct.get $assoc 2 (local.get $a)))
            (br $loop))))

   (func $caml_named_value (export "caml_named_value")
      (param $s (ref eq)) (result (ref null eq))
      (block $not_found
         (return
            (struct.get $assoc 1
               (br_on_null $not_found
                  (call $assoc_find
                     (local.get $s)
                     (array.get $assoc_array (global.get $named_value_table)
                        (i32.rem_u
                           (i31.get_s
                              (ref.cast (ref i31)
                                 (call $caml_string_hash
                                    (ref.i31 (i32.const 0)) (local.get $s))))
                           (global.get $Named_value_size))))))))
      (return (ref.null eq)))

   (func (export "caml_register_named_value")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (local $h i32)
      (local $r (ref null $assoc))
      (local.set $h
         (i32.rem_u
            (i31.get_s
               (ref.cast (ref i31)
                  (call $caml_string_hash
                     (ref.i31 (i32.const 0)) (local.get 0))))
            (global.get $Named_value_size)))
      (local.set $r
         (array.get $assoc_array
            (global.get $named_value_table) (local.get $h)))
      (block $not_found
         (struct.set $assoc 1
            (br_on_null $not_found
               (call $assoc_find (local.get 0) (local.get $r)))
            (local.get 1))
         (return (ref.i31 (i32.const 0))))
      (array.set $assoc_array
         (global.get $named_value_table) (local.get $h)
         (struct.new $assoc
            (ref.cast (ref $bytes) (local.get 0))
            (local.get 1) (local.get $r)))
      (ref.i31 (i32.const 0)))

   ;; Used only for testing (tests-jsoo/bin), but inconvenient to pull out
   (func (export "caml_unregister_named_value")
      (param $name (ref eq)) (result (ref eq))
      (local $h i32)
      (local $r (ref null $assoc)) (local $a (ref $assoc))
      (local.set $h
         (i32.rem_u
            (i31.get_s
               (ref.cast (ref i31)
                  (call $caml_string_hash
                     (ref.i31 (i32.const 0)) (local.get $name))))
            (global.get $Named_value_size)))
      (local.set $r
         (array.get $assoc_array
            (global.get $named_value_table) (local.get $h)))
      (block $done
         (local.set $a (br_on_null $done (local.get $r)))
         (local.set $r (struct.get $assoc 2 (local.get $a)))
         (if (i31.get_u
                (ref.cast (ref i31)
                    (call $caml_string_equal
                       (local.get $name)
                       (struct.get $assoc 0 (local.get $a)))))
            (then
               (array.set $assoc_array
                  (global.get $named_value_table) (local.get $h)
                  (local.get $r))
               (br $done)))
         (loop $loop
            (local.set $a (br_on_null $done (local.get $r)))
            (if (i31.get_u
                   (ref.cast (ref i31)
                       (call $caml_string_equal
                          (local.get $name)
                          (struct.get $assoc 0 (local.get $a)))))
               (then
                  (struct.set $assoc 2 (local.get $r)
                     (struct.get $assoc 2 (local.get $a)))
                  (br $done)))
            (local.set $r (struct.get $assoc 2 (local.get $a)))
            (br $loop)))
      (ref.i31 (i32.const 0)))

   (global $caml_global_data (export "caml_global_data") (mut (ref $block))
      (array.new $block (ref.i31 (i32.const 0)) (i32.const 13)))

   ;; Grow caml_global_data so that index $min_idx is valid.
   (func $grow_global_data (param $min_idx i32)
      (local $old (ref $block))
      (local $new_len i32)
      (local.set $old (global.get $caml_global_data))
      ;; new length = min_idx + 1, but at least double the old size
      (local.set $new_len
         (i32.add (local.get $min_idx) (i32.const 1)))
      (if (i32.lt_u (local.get $new_len)
                     (i32.shl (array.len (local.get $old)) (i32.const 1)))
         (then
            (local.set $new_len
               (i32.shl (array.len (local.get $old)) (i32.const 1)))))
      (global.set $caml_global_data
         (array.new $block (ref.i31 (i32.const 0)) (local.get $new_len)))
      (array.copy $block $block
         (global.get $caml_global_data) (i32.const 0)
         (local.get $old) (i32.const 0)
         (array.len (local.get $old))))

   ;; Link info: a single mutable block holding all linker-provided data.
   ;; Layout (OCaml block, field 0 = tag):
   ;;   field 1: sections  — bytesections record { symb; crcs; prim; dlpt }
   ;;   field 2: symbols   — array of (string * int) pairs for name→index mapping
   ;;   field 3: prim_count — int (number of registered primitives)
   ;;   field 4: aliases    — (string * string) list of primitive aliases
   ;; All fields default to 0 (unit/empty).
   (global $link_info (export "link_info") (mut (ref $block))
      (array.new_fixed $block 5
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))
         (ref.i31 (i32.const 0))))

   ;; Field indices for $link_info (after tag at 0)
   (global $LINK_INFO_SECTIONS i32 (i32.const 1))
   (global $LINK_INFO_SYMBOLS i32 (i32.const 2))
   (global $LINK_INFO_PRIM_COUNT i32 (i32.const 3))
   (global $LINK_INFO_ALIASES i32 (i32.const 4))

   ;; Next available index for dynamically loaded modules not in the
   ;; static symbols table. Initialized when symbols are set.
   (global $next_idx (mut i32) (i32.const 0))

   (func (export "caml_set_link_info")
      (param $info (ref eq)) (result (ref eq))
      (local $arr (ref $block))
      (local $pair (ref $block))
      (local $j i32)
      (local $n i32)
      (local $h i32)
      (local $max i32)
      (local $idx i32)
      (local $key (ref $bytes))
      (global.set $link_info (ref.cast (ref $block) (local.get $info)))
      ;; Compute next_idx from symbols field
      (if (ref.test (ref $block)
             (array.get $block (global.get $link_info)
                (global.get $LINK_INFO_SYMBOLS)))
         (then
            (local.set $arr
               (ref.cast (ref $block)
                  (array.get $block (global.get $link_info)
                     (global.get $LINK_INFO_SYMBOLS))))
            (local.set $max (i32.const -1))
            ;; n = number of symbol entries (array.len - 1 to skip tag)
            (local.set $n
               (i32.sub (array.len (local.get $arr)) (i32.const 1)))
            ;; Create hash table of size max(n, 1)
            (if (i32.gt_u (local.get $n) (i32.const 0))
               (then
                  (global.set $symbol_table_size (local.get $n))
                  (global.set $symbol_table
                     (array.new $assoc_array
                        (ref.null $assoc) (local.get $n)))))
            (local.set $j (i32.const 1))
            (block $done
               (loop $loop
                  (br_if $done
                     (i32.ge_u (local.get $j) (array.len (local.get $arr))))
                  ;; pair = [tag, global_name, index]
                  ;; global_name = [is_predef, name]
                  (local.set $pair
                     (ref.cast (ref $block)
                        (array.get $block (local.get $arr) (local.get $j))))
                  (local.set $idx
                     (i31.get_u
                        (ref.cast (ref i31)
                           (array.get $block (local.get $pair) (i32.const 2)))))
                  (if (i32.gt_s (local.get $idx) (local.get $max))
                     (then (local.set $max (local.get $idx))))
                  ;; Build key from global_name
                  (local.set $key
                     (call $make_symbol_key
                        ;; is_predef = tag of global_name block
                        (i31.get_u
                           (ref.cast (ref i31)
                              (array.get $block
                                 (ref.cast (ref $block)
                                    (array.get $block (local.get $pair) (i32.const 1)))
                                 (i32.const 0))))
                        (ref.cast (ref $bytes)
                           (array.get $block
                              (ref.cast (ref $block)
                                 (array.get $block (local.get $pair) (i32.const 1)))
                              (i32.const 1)))))
                  ;; Insert into symbol hash table
                  (local.set $h
                     (i32.rem_u
                        (i31.get_s
                           (ref.cast (ref i31)
                              (call $caml_string_hash
                                 (ref.i31 (i32.const 0))
                                 (local.get $key))))
                        (global.get $symbol_table_size)))
                  (array.set $assoc_array
                     (global.get $symbol_table) (local.get $h)
                     (struct.new $assoc
                        (local.get $key)
                        (array.get $block (local.get $pair) (i32.const 2))
                        (array.get $assoc_array
                           (global.get $symbol_table) (local.get $h))))
                  (local.set $j (i32.add (local.get $j) (i32.const 1)))
                  (br $loop)))
            (global.set $next_idx (i32.add (local.get $max) (i32.const 1)))))
      (ref.i31 (i32.const 0)))

   ;; Relocation callback: set by wasm_of_ocaml_compiler_dynlink when
   ;; the toplevel is active. Maps a name (string) to its symtable index.
   ;; Similar to jsoo_toplevel_reloc in the JS runtime.
   (global $toplevel_reloc (export "toplevel_reloc") (mut (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "wasm_toplevel_init_reloc")
      (param $f (ref eq)) (result (ref eq))
      (global.set $toplevel_reloc (local.get $f))
      (ref.i31 (i32.const 0)))

   ;; Look up a name in the symbol hash table and return the symtable index.
   ;; Returns -1 if not found or if symbols is not set.
   (func $lookup_symbol (param $name (ref eq)) (param $kind i32) (result i32)
      (local $key (ref $bytes))
      (if (ref.test (ref i31)
             (array.get $block (global.get $link_info)
                (global.get $LINK_INFO_SYMBOLS)))
         (then (return (i32.const -1))))
      (local.set $key
         (call $make_symbol_key (local.get $kind)
            (ref.cast (ref $bytes) (local.get $name))))
      (block $not_found
         (return
            (i31.get_u
               (ref.cast (ref i31)
                  (struct.get $assoc 1
                     (br_on_null $not_found
                        (call $assoc_find
                           (local.get $key)
                           (array.get $assoc_array
                              (global.get $symbol_table)
                              (i32.rem_u
                                 (i31.get_s
                                    (ref.cast (ref i31)
                                       (call $caml_string_hash
                                          (ref.i31 (i32.const 0))
                                          (local.get $key))))
                                 (global.get $symbol_table_size))))))))))
      (i32.const -1))

   (global $caml_register_global_idx (mut i32) (i32.const 0))

   (func (export "caml_register_global_by_index")
      (param $v (ref eq)) (param $idx (ref eq)) (result (ref eq))
      (local $i i32)
      ;; caml_global_data is a $block: index 0 is the tag, data starts at 1
      (local.set $i
         (i32.add (i31.get_u (ref.cast (ref i31) (local.get $idx))) (i32.const 1)))
      (if (i32.ge_u (local.get $i) (array.len (global.get $caml_global_data)))
         (then
            (call $grow_global_data (local.get $i))))
      (array.set $block (global.get $caml_global_data)
         (local.get $i) (local.get $v))
      (ref.i31 (i32.const 0)))

   (func $do_register_global
      (param $v (ref eq)) (param $name (ref eq)) (param $is_predef i32)
      (result (ref eq))
      (local $i i32)
      (local $idx i32)
      ;; Try to resolve the name to a symtable index.
      ;; 1. toplevel_reloc callback (set by wasm_of_ocaml_compiler_dynlink)
      (if (i32.eqz (ref.test (ref i31) (global.get $toplevel_reloc)))
         (then
            (local.set $i
               (i32.add
                  (i31.get_u
                     (ref.cast (ref i31)
                        (call $caml_callback_1
                           (global.get $toplevel_reloc)
                           ;; Construct global_name: [tag, name]
                           (array.new_fixed $block 2
                              (ref.i31 (local.get $is_predef))
                              (local.get $name)))))
                  (i32.const 1))))
         (else
            ;; 2. static symbols array (set by linker in link_info)
            (if (i32.eqz (ref.test (ref i31)
                   (array.get $block (global.get $link_info)
                      (global.get $LINK_INFO_SYMBOLS))))
               (then
                  (local.set $idx
                     (call $lookup_symbol
                        (local.get $name)
                        (local.get $is_predef)))
                  (if (i32.ge_s (local.get $idx) (i32.const 0))
                     (then
                        (local.set $i
                           (i32.add (local.get $idx) (i32.const 1))))
                     (else
                        ;; Not found in symbols: assign a new index
                        (local.set $i
                           (i32.add (global.get $next_idx) (i32.const 1)))
                        (global.set $next_idx
                           (i32.add (global.get $next_idx) (i32.const 1))))))
               (else
                  ;; 3. No relocation info: nothing to do.
                  ;; caml_global_data is populated by caml_register_global_by_index.
                  (return (ref.i31 (i32.const 0)))))))
      (if (i32.ge_u (local.get $i) (array.len (global.get $caml_global_data)))
         (then
            (call $grow_global_data (local.get $i))))
      (array.set $block (global.get $caml_global_data)
         (local.get $i) (local.get $v))
      (ref.i31 (i32.const 0)))

   (func (export "caml_register_global")
      (param $v (ref eq)) (param $name (ref eq)) (result (ref eq))
      (call $do_register_global (local.get $v) (local.get $name) (i32.const 0)))

   (func (export "caml_register_global_predef")
      (param $v (ref eq)) (param $name (ref eq)) (result (ref eq))
      (call $do_register_global (local.get $v) (local.get $name) (i32.const 1)))

   (func (export "caml_get_global_data") (param (ref eq)) (result (ref eq))
      (global.get $caml_global_data))

   (type $func (func (result (ref eq))))

   (@string $fatal_error "Fatal error: exception ")
   (@string $handle_uncaught_exception "Printexc.handle_uncaught_exception")
   (@string $do_at_exit "Pervasives.do_at_exit")

   (global $uncaught_exception (mut externref) (ref.null extern))

   (func $reraise_exception (result (ref eq))
      (throw $javascript_exception (global.get $uncaught_exception))
      (ref.i31 (i32.const 0)))

   (func (export "caml_handle_uncaught_exception") (param $exn externref)
      (global.set $uncaught_exception (local.get $exn))
      (call $caml_main (ref.func $reraise_exception)))

   (func $caml_main (export "caml_main") (param $start (ref func))
      (local $exn (ref eq))
      (local $msg (ref eq))
      (try
         (do
            (drop (call_ref $func (ref.cast (ref $func) (local.get $start)))))
         (catch $ocaml_exit)
         (catch $ocaml_exception
            (local.set $exn (pop (ref eq)))
            (block $exit
               (block $not_registered
                  (try
                     (do
                        (drop
                           (call $caml_callback_2
                              (br_on_null $not_registered
                                 (call $caml_named_value
                                     (global.get $handle_uncaught_exception)))
                              (local.get $exn)
                              (ref.i31 (i32.const 0)))))
                     (catch $ocaml_exit
                        (return)))
                  (br $exit))
               (block $null
                  (drop
                     (call $caml_callback_1
                        (br_on_null $null
                           (call $caml_named_value (global.get $do_at_exit)))
                        (ref.i31 (i32.const 0)))))
               (local.set $msg
                  (call $caml_string_concat
                     (global.get $fatal_error)
                     (call $caml_string_concat
                        (call $caml_format_exception (local.get $exn))
                        (@string "\n"))))
               (call $write (i32.const 2)
                  (call $unwrap
                     (call $caml_jsstring_of_string (local.get $msg)))))
            (call $exit (i32.const 2)))))

   (func (export "caml_with_async_exns") (param $f (ref eq)) (result (ref eq))
      (return_call $caml_callback_1 (local.get $f) (ref.i31 (i32.const 0))))
)
