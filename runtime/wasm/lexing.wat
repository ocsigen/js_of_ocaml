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
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))

   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))

   (func $get (param $a (ref eq)) (param $i i32) (result i32)
      (local $s (ref $bytes))
      (local.set $s (ref.cast (ref $bytes) (local.get $a)))
      (local.set $i (i32.add (local.get $i) (local.get $i)))
      (i32.extend16_s
         (i32.or (array.get_u $bytes (local.get $s) (local.get $i))
            (i32.shl
               (array.get_u $bytes (local.get $s)
                  (i32.add (local.get $i) (i32.const 1)))
               (i32.const 8)))))

   (global $lex_buffer i32 (i32.const 2))
   (global $lex_buffer_len i32 (i32.const 3))
   (global $lex_start_pos i32 (i32.const 5))
   (global $lex_curr_pos i32 (i32.const 6))
   (global $lex_last_pos i32 (i32.const 7))
   (global $lex_last_action i32 (i32.const 8))
   (global $lex_eof_reached i32 (i32.const 9))
   (global $lex_mem i32 (i32.const 10))
   (global $lex_base i32 (i32.const 1))
   (global $lex_backtrk i32 (i32.const 2))
   (global $lex_default i32 (i32.const 3))
   (global $lex_trans i32 (i32.const 4))
   (global $lex_check i32 (i32.const 5))
   (global $lex_base_code i32 (i32.const 6))
   (global $lex_backtrk_code i32 (i32.const 7))
   (global $lex_default_code i32 (i32.const 8))
   (global $lex_trans_code i32 (i32.const 9))
   (global $lex_check_code i32 (i32.const 10))
   (global $lex_code i32 (i32.const 11))

   (@string $lexing_empty_token "lexing: empty token")

   (func (export "caml_lex_engine")
      (param $vtbl (ref eq)) (param $start_state (ref eq))
      (param $vlexbuf (ref eq))
      (result (ref eq))
      (local $tbl (ref $block))
      (local $lexbuf (ref $block))
      (local $c i32)
      (local $state i32)
      (local $buffer (ref $bytes))
      (local $vpos (ref eq)) (local $action (ref eq))
      (local $pos i32) (local $base i32) (local $backtrk i32)
      (local $lex_base (ref $bytes))
      (local $lex_backtrk (ref $bytes))
      (local $lex_check (ref $bytes))
      (local $lex_check_code (ref $bytes))
      (local $lex_trans (ref $bytes))
      (local $lex_default (ref $bytes))
      (local.set $tbl (ref.cast (ref $block) (local.get $vtbl)))
      (local.set $lexbuf (ref.cast (ref $block) (local.get $vlexbuf)))
      (local.set $state
         (i31.get_s (ref.cast (ref i31) (local.get $start_state))))
      (local.set $buffer
         (ref.cast (ref $bytes)
            (array.get $block (local.get $lexbuf) (global.get $lex_buffer))))
      (if (i32.ge_s (local.get $state) (i32.const 0))
         (then
            (local.set $vpos
               (array.get $block (local.get $lexbuf) (global.get $lex_curr_pos)))
            (array.set $block (local.get $lexbuf) (global.get $lex_last_pos)
               (local.get $vpos))
            (array.set $block (local.get $lexbuf) (global.get $lex_start_pos)
               (local.get $vpos))
            (array.set $block (local.get $lexbuf) (global.get $lex_last_action)
               (ref.i31 (i32.const -1))))
         (else
            (local.set $state (i32.sub (i32.const -1) (local.get $state)))))
      (local.set $lex_base
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_base))))
      (local.set $lex_backtrk
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_backtrk))))
      (local.set $lex_check
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_check))))
      (local.set $lex_check_code
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_check_code))))
      (local.set $lex_trans
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_trans))))
      (local.set $lex_default
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_default))))
      (loop $loop
         (local.set $base (call $get (local.get $lex_base) (local.get $state)))
         (if (i32.lt_s (local.get $base) (i32.const 0))
            (then
               (return (ref.i31 (i32.sub (i32.const -1) (local.get $base))))))
         (local.set $backtrk
            (call $get (local.get $lex_backtrk) (local.get $state)))
         (if (i32.ge_s (local.get $backtrk) (i32.const 0))
            (then
               (array.set $block (local.get $lexbuf) (global.get $lex_last_pos)
                  (array.get $block (local.get $lexbuf)
                     (global.get $lex_curr_pos)))
               (array.set $block (local.get $lexbuf)
                  (global.get $lex_last_action)
                  (ref.i31 (local.get $backtrk)))))
         (if (i32.ge_s
                (i31.get_s
                   (ref.cast (ref i31)
                      (array.get $block (local.get $lexbuf)
                         (global.get $lex_curr_pos))))
                (i31.get_s
                   (ref.cast (ref i31)
                      (array.get $block (local.get $lexbuf)
                         (global.get $lex_buffer_len)))))
            (then
               (if (ref.eq
                      (array.get $block (local.get $lexbuf)
                         (global.get $lex_eof_reached))
                      (ref.i31 (i32.const 0)))
                  (then
                     (return
                        (ref.i31 (i32.sub (i32.const -1) (local.get $state)))))
                  (else
                     (local.set $c (i32.const 256)))))
            (else
               (local.set $pos
                  (i31.get_u
                     (ref.cast (ref i31)
                        (array.get $block (local.get $lexbuf)
                           (global.get $lex_curr_pos)))))
               (local.set $c
                  (array.get_u $bytes (local.get $buffer) (local.get $pos)))
               (array.set $block (local.get $lexbuf) (global.get $lex_curr_pos)
                  (ref.i31 (i32.add (local.get $pos) (i32.const 1))))))
         (if (i32.eq
                (call $get (local.get $lex_check)
                   (i32.add (local.get $base) (local.get $c)))
                (local.get $state))
            (then
               (local.set $state
                  (call $get (local.get $lex_trans)
                     (i32.add (local.get $base) (local.get $c)))))
            (else
               (local.set $state
                  (call $get (local.get $lex_default) (local.get $state)))))
         (if (i32.lt_s (local.get $state) (i32.const 0))
            (then
               (array.set $block (local.get $lexbuf) (global.get $lex_curr_pos)
                  (array.get $block (local.get $lexbuf)
                     (global.get $lex_last_pos)))
               (local.set $action
                  (array.get $block (local.get $lexbuf)
                     (global.get $lex_last_action)))
               (if (ref.eq (local.get $action) (ref.i31 (i32.const -1)))
                  (then (call $caml_failwith (global.get $lexing_empty_token))))
               (return (local.get $action))))
         (if (i32.eq (local.get $c) (i32.const 256))
            (then
               (array.set $block (local.get $lexbuf)
                  (global.get $lex_eof_reached)
                  (ref.i31 (i32.const 0)))))
         (br $loop)))

   (func $run_mem
      (param $s (ref $bytes)) (param $i i32) (param $lexbuf (ref $block))
      (param $curr_pos (ref eq))
      (local $dst i32) (local $src i32)
      (local $mem (ref $block))
      (local.set $mem
         (ref.cast (ref $block)
            (array.get $block (local.get $lexbuf) (global.get $lex_mem))))
      (loop $loop
         (local.set $dst (array.get_u $bytes (local.get $s) (local.get $i)))
         (if (i32.eq (local.get $dst) (i32.const 0xff))
            (then (return)))
         (local.set $src
            (array.get_u $bytes (local.get $s)
              (i32.add (local.get $i) (i32.const 1))))
         (local.set $i (i32.add (local.get $i) (i32.const 2)))
         (array.set $block (local.get $mem)
            (i32.add (local.get $dst) (i32.const 1))
            (if (result (ref eq)) (i32.eq (local.get $src) (i32.const 0xff))
               (then
                  (local.get $curr_pos))
               (else
                  (array.get $block (local.get $mem)
                     (i32.add (local.get $src) (i32.const 1))))))
         (br $loop)))

   (func $run_tag
      (param $s (ref $bytes)) (param $i i32) (param $lexbuf (ref $block))
      (return_call $run_mem (local.get $s) (local.get $i) (local.get $lexbuf)
         (ref.i31 (i32.const -1))))

   (func (export "caml_new_lex_engine")
      (param $vtbl (ref eq)) (param $start_state (ref eq))
      (param $vlexbuf (ref eq))
      (result (ref eq))
      (local $tbl (ref $block))
      (local $lexbuf (ref $block))
      (local $c i32)
      (local $state i32) (local $pstate i32)
      (local $buffer (ref $bytes))
      (local $vpos (ref eq)) (local $action (ref eq))
      (local $pos i32) (local $base i32) (local $backtrk i32)
      (local $pc_off i32) (local $base_code i32)
      (local $lex_code (ref $bytes))
      (local $lex_base (ref $bytes))
      (local $lex_base_code (ref $bytes))
      (local $lex_backtrk (ref $bytes))
      (local $lex_backtrk_code (ref $bytes))
      (local $lex_check (ref $bytes))
      (local $lex_check_code (ref $bytes))
      (local $lex_trans (ref $bytes))
      (local $lex_trans_code (ref $bytes))
      (local $lex_default (ref $bytes))
      (local $lex_default_code (ref $bytes))
      (local.set $tbl (ref.cast (ref $block) (local.get $vtbl)))
      (local.set $lexbuf (ref.cast (ref $block) (local.get $vlexbuf)))
      (local.set $state
         (i31.get_s (ref.cast (ref i31) (local.get $start_state))))
      (local.set $buffer
         (ref.cast (ref $bytes)
            (array.get $block (local.get $lexbuf) (global.get $lex_buffer))))
      (if (i32.ge_s (local.get $state) (i32.const 0))
         (then
            (local.set $vpos
               (array.get $block (local.get $lexbuf) (global.get $lex_curr_pos)))
            (array.set $block (local.get $lexbuf) (global.get $lex_last_pos)
               (local.get $vpos))
            (array.set $block (local.get $lexbuf) (global.get $lex_start_pos)
               (local.get $vpos))
            (array.set $block (local.get $lexbuf) (global.get $lex_last_action)
               (ref.i31 (i32.const -1))))
         (else
            (local.set $state (i32.sub (i32.const -1) (local.get $state)))))
      (local.set $lex_code
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_code))))
      (local.set $lex_base
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_base))))
      (local.set $lex_base_code
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_base_code))))
      (local.set $lex_backtrk
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_backtrk))))
      (local.set $lex_backtrk_code
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_backtrk_code))))
      (local.set $lex_check
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_check))))
      (local.set $lex_check_code
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_check_code))))
      (local.set $lex_trans
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_trans))))
      (local.set $lex_trans_code
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_trans_code))))
      (local.set $lex_default
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_default))))
      (local.set $lex_default_code
         (ref.cast (ref $bytes)
            (array.get $block (local.get $tbl) (global.get $lex_default_code))))
      (loop $loop
         (local.set $base (call $get (local.get $lex_base) (local.get $state)))
         (if (i32.lt_s (local.get $base) (i32.const 0))
            (then
               (local.set $pc_off
                  (call $get (local.get $lex_base_code) (local.get $state)))
               (call $run_tag (local.get $lex_code) (local.get $pc_off)
                  (local.get $lexbuf))
               (return (ref.i31 (i32.sub (i32.const -1) (local.get $base))))))
         (local.set $backtrk
            (call $get (local.get $lex_backtrk) (local.get $state)))
         (if (i32.ge_s (local.get $backtrk) (i32.const 0))
            (then
               (local.set $pc_off
                  (call $get (local.get $lex_backtrk_code) (local.get $state)))
               (call $run_tag (local.get $lex_code) (local.get $pc_off)
                  (local.get $lexbuf))
               (array.set $block (local.get $lexbuf) (global.get $lex_last_pos)
                  (array.get $block (local.get $lexbuf)
                     (global.get $lex_curr_pos)))
               (array.set $block (local.get $lexbuf)
                  (global.get $lex_last_action)
                  (ref.i31 (local.get $backtrk)))))
         (if (i32.ge_s
                (i31.get_s
                   (ref.cast (ref i31)
                      (array.get $block (local.get $lexbuf)
                         (global.get $lex_curr_pos))))
                (i31.get_s
                   (ref.cast (ref i31)
                      (array.get $block (local.get $lexbuf)
                         (global.get $lex_buffer_len)))))
            (then
               (if (ref.eq
                      (array.get $block (local.get $lexbuf)
                         (global.get $lex_eof_reached))
                      (ref.i31 (i32.const 0)))
                  (then
                     (return
                        (ref.i31 (i32.sub (i32.const -1) (local.get $state)))))
                  (else
                     (local.set $c (i32.const 256)))))
            (else
               (local.set $pos
                  (i31.get_u
                     (ref.cast (ref i31)
                        (array.get $block (local.get $lexbuf)
                           (global.get $lex_curr_pos)))))
               (local.set $c
                  (array.get_u $bytes (local.get $buffer) (local.get $pos)))
               (array.set $block (local.get $lexbuf) (global.get $lex_curr_pos)
                  (ref.i31 (i32.add (local.get $pos) (i32.const 1))))))
         (local.set $pstate (local.get $state))
         (if (i32.eq
                (call $get (local.get $lex_check)
                   (i32.add (local.get $base) (local.get $c)))
                (local.get $state))
            (then
               (local.set $state
                  (call $get (local.get $lex_trans)
                     (i32.add (local.get $base) (local.get $c)))))
            (else
               (local.set $state
                  (call $get (local.get $lex_default) (local.get $state)))))
         (if (i32.lt_s (local.get $state) (i32.const 0))
            (then
               (array.set $block (local.get $lexbuf) (global.get $lex_curr_pos)
                  (array.get $block (local.get $lexbuf)
                     (global.get $lex_last_pos)))
               (local.set $action
                  (array.get $block (local.get $lexbuf)
                     (global.get $lex_last_action)))
               (if (ref.eq (local.get $action) (ref.i31 (i32.const -1)))
                  (then (call $caml_failwith (global.get $lexing_empty_token))))
               (return (local.get $action))))
         (local.set $base_code
            (call $get (local.get $lex_base_code) (local.get $pstate)))
         (local.set $pc_off
            (if (result i32)
                (i32.eq
                   (call $get (local.get $lex_check_code)
                      (i32.add (local.get $base_code) (local.get $c)))
                   (local.get $pstate))
               (then
                  (call $get (local.get $lex_trans_code)
                     (i32.add (local.get $base_code) (local.get $c))))
               (else
                  (call $get (local.get $lex_default_code)
                     (local.get $pstate)))))
         (call $run_mem (local.get $lex_code) (local.get $pc_off)
            (local.get $lexbuf)
            (array.get $block (local.get $lexbuf) (global.get $lex_curr_pos)))
         (if (i32.eq (local.get $c) (i32.const 256))
            (then
               (array.set $block (local.get $lexbuf)
                  (global.get $lex_eof_reached)
                  (ref.i31 (i32.const 0)))))
         (br $loop)))
)
