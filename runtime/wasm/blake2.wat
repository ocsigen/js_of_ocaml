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
(@if (>= $ocaml_version (5 2 0))
(@then
   (import "blake2" "blake2_memory" (memory 1))
   (import "blake2" "blake2_state_buf" (func $state_buf (result i32)))
   (import "blake2" "blake2_key_buf"   (func $key_buf   (result i32)))
   (import "blake2" "blake2_out_buf"   (func $out_buf   (result i32)))
   (import "blake2" "blake2_chunk_buf" (func $chunk_buf (result i32)))
   (import "blake2" "blake2_chunk_buf_size"
      (func $chunk_buf_size (result i32)))
   (import "blake2" "blake2_init"
      (func $blake2_init (param i32 i32)))
   (import "blake2" "blake2_update"
      (func $blake2_update (param i32)))
   (import "blake2" "blake2_finalize"
      (func $blake2_finalize (param i32)))

   (type $bytes (array (mut i8)))

   ;; Must match BLAKE2_STATE_SIZE in blake2_wrap.c.  A _Static_assert there
   ;; rejects shrinking blake2b_state past this value at C compile time.
   (global $blake2_state_size i32 (i32.const 248))

   ;; BLAKE2B_KEYBYTES: the width of $key_buf and the maximum key length.
   (global $blake2_key_size i32 (i32.const 64))

   ;; Blit a slice of a GC byte array into linear memory.
   (func $bytes_to_mem
      (param $src (ref $bytes)) (param $src_ofs i32)
      (param $dst i32) (param $len i32)
      (local $i i32)
      (block $done
         (loop $cont
            (br_if $done (i32.eq (local.get $i) (local.get $len)))
            (i32.store8
               (i32.add (local.get $dst) (local.get $i))
               (array.get_u $bytes (local.get $src)
                  (i32.add (local.get $src_ofs) (local.get $i))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $cont))))

   ;; Blit a slice of linear memory into a GC byte array.
   (func $mem_to_bytes
      (param $src i32) (param $dst (ref $bytes)) (param $dst_ofs i32)
      (param $len i32)
      (local $i i32)
      (block $done
         (loop $cont
            (br_if $done (i32.eq (local.get $i) (local.get $len)))
            (array.set $bytes (local.get $dst)
               (i32.add (local.get $dst_ofs) (local.get $i))
               (i32.load8_u
                  (i32.add (local.get $src) (local.get $i))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $cont))))

   ;; Allocate a fresh state byte array and run blake2_init into it.
   (func $do_create
      (param $hashlen i32) (param $key (ref $bytes)) (result (ref $bytes))
      (local $state (ref $bytes)) (local $keylen i32)
      (local.set $state
         (array.new $bytes (i32.const 0) (global.get $blake2_state_size)))
      ;; BLAKE2b keys are at most BLAKE2B_KEYBYTES (64), the width of $key_buf.
      ;; Truncate a longer key to its 64-byte prefix (matching the JS runtime)
      ;; so the blit and blake2_init stay in bounds.
      (local.set $keylen (array.len (local.get $key)))
      (if (i32.gt_u (local.get $keylen) (global.get $blake2_key_size))
         (then (local.set $keylen (global.get $blake2_key_size))))
      (call $bytes_to_mem
         (local.get $key) (i32.const 0)
         (call $key_buf) (local.get $keylen))
      (call $blake2_init (local.get $hashlen) (local.get $keylen))
      (call $mem_to_bytes
         (call $state_buf) (local.get $state) (i32.const 0)
         (global.get $blake2_state_size))
      (local.get $state))

   ;; Feed input through the fixed-size chunk buffer.
   (func $do_update
      (param $state (ref $bytes)) (param $buf (ref $bytes))
      (param $ofs i32) (param $len i32)
      (local $chunk i32) (local $chunk_max i32)
      (call $bytes_to_mem
         (local.get $state) (i32.const 0)
         (call $state_buf) (global.get $blake2_state_size))
      (local.set $chunk_max (call $chunk_buf_size))
      (block $done
         (loop $cont
            (br_if $done (i32.eqz (local.get $len)))
            (local.set $chunk
               (select
                  (local.get $chunk_max) (local.get $len)
                  (i32.gt_u (local.get $len) (local.get $chunk_max))))
            (call $bytes_to_mem
               (local.get $buf) (local.get $ofs)
               (call $chunk_buf) (local.get $chunk))
            (call $blake2_update (local.get $chunk))
            (local.set $ofs (i32.add (local.get $ofs) (local.get $chunk)))
            (local.set $len (i32.sub (local.get $len) (local.get $chunk)))
            (br $cont)))
      (call $mem_to_bytes
         (call $state_buf) (local.get $state) (i32.const 0)
         (global.get $blake2_state_size)))

   ;; Run blake2_finalize and copy the digest into a fresh byte array.
   (func $do_final
      (param $state (ref $bytes)) (param $hashlen i32) (result (ref $bytes))
      (local $output (ref $bytes))
      (call $bytes_to_mem
         (local.get $state) (i32.const 0)
         (call $state_buf) (global.get $blake2_state_size))
      (call $blake2_finalize (local.get $hashlen))
      (local.set $output
         (array.new $bytes (i32.const 0) (local.get $hashlen)))
      (call $mem_to_bytes
         (call $out_buf) (local.get $output) (i32.const 0)
         (local.get $hashlen))
      (local.get $output))

   (func (export "caml_blake2_create")
      (param $hashlen (ref eq)) (param $key (ref eq)) (result (ref eq))
      (call $do_create
         (i31.get_u (ref.cast (ref i31) (local.get $hashlen)))
         (ref.cast (ref $bytes) (local.get $key))))

   (func (export "caml_blake2_update")
      (param $ctx (ref eq)) (param $buf (ref eq)) (param $ofs (ref eq))
      (param $len (ref eq)) (result (ref eq))
      (call $do_update
         (ref.cast (ref $bytes) (local.get $ctx))
         (ref.cast (ref $bytes) (local.get $buf))
         (i31.get_u (ref.cast (ref i31) (local.get $ofs)))
         (i31.get_u (ref.cast (ref i31) (local.get $len))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_blake2_final")
      (param $ctx (ref eq)) (param $hashlen (ref eq)) (result (ref eq))
      (call $do_final
         (ref.cast (ref $bytes) (local.get $ctx))
         (i31.get_u (ref.cast (ref i31) (local.get $hashlen)))))

   (func (export "caml_blake2_string") (export "caml_blake2_bytes")
      (param $hashlen (ref eq)) (param $key (ref eq)) (param $buf (ref eq))
      (param $ofs (ref eq)) (param $len (ref eq)) (result (ref eq))
      (local $state (ref $bytes))
      (local $hl i32)
      (local.set $hl
         (i31.get_u (ref.cast (ref i31) (local.get $hashlen))))
      (local.set $state
         (call $do_create
            (local.get $hl)
            (ref.cast (ref $bytes) (local.get $key))))
      (call $do_update
         (local.get $state)
         (ref.cast (ref $bytes) (local.get $buf))
         (i31.get_u (ref.cast (ref i31) (local.get $ofs)))
         (i31.get_u (ref.cast (ref i31) (local.get $len))))
      (call $do_final (local.get $state) (local.get $hl)))
))
)
