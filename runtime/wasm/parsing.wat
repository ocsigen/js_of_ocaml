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
   (import "io" "caml_stderr" (global $caml_stderr (mut (ref eq))))
   (import "io" "caml_ml_output"
      (func $caml_ml_output
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "io" "caml_ml_flush"
      (func $caml_ml_flush (param (ref eq)) (result (ref eq))))
   (import "ints" "caml_format_int"
      (func $caml_format_int
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "float" "caml_format_float"
      (func $caml_format_float
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "string" "caml_string_length"
      (func $caml_string_length (param (ref eq)) (result i32)))

   (type $float (struct (field f64)))
   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $string (struct (field anyref)))

(@if use-js-string
(@then
   (import "wasm:js-string" "substring"
      (func $string_substring (param externref i32 i32) (result (ref extern))))
   (import "wasm:js-string" "charCodeAt"
      (func $string_get (param externref i32) (result i32)))

   (func $string_sub
      (param $s externref) (param $i i32) (param $l i32) (result (ref eq))
      (struct.new $string
         (any.convert_extern
            (call $string_substring
               (local.get $s)
               (local.get $i)
               (i32.add (local.get $i) (local.get $l))))))

   (func $string_val (param $s (ref eq)) (result externref)
      (extern.convert_any
         (struct.get $string 0 (ref.cast (ref $string) (local.get $s)))))
)
(@else
   (func $string_sub
      (param $s (ref $bytes)) (param $i i32) (param $l i32) (result (ref eq))
      (local $s' (ref $bytes))
      (local.set $s' (array.new $bytes (i32.const 0) (local.get $l)))
      (array.copy $bytes $bytes
         (local.get $s') (i32.const 0)
         (local.get $s) (local.get $i)
         (local.get $l))
      (local.get $s'))
   (func $string_get (param $s (ref $bytes)) (param $i i32) (result i32)
      (array.get $bytes (local.get $s) (local.get $i)))
   (func $string_val (param $s (ref eq)) (result (ref $bytes))
      (ref.cast (ref $bytes) (local.get $s)))
))

   (func $get
(@if use-js-string
(@then
      (param $s externref)
)
(@else
      (param $s (ref $bytes))
))
      (param $i i32) (result i32)
      (local.set $i (i32.add (local.get $i) (local.get $i)))
      (i32.extend16_s
         (i32.or (call $string_get (local.get $s) (local.get $i))
            (i32.shl
               (call $string_get (local.get $s)
                  (i32.add (local.get $i) (i32.const 1)))
               (i32.const 8)))))

   (global $caml_parser_trace (mut i32) (i32.const 0))

   (global $ERRCODE i32 (i32.const 256))

   (global $START i32 (i32.const 0))
   (global $TOKEN_READ i32 (i32.const 1))
   (global $STACKS_GROWN_1 i32 (i32.const 2))
   (global $STACKS_GROWN_2 i32 (i32.const 3))
   (global $SEMANTIC_ACTION_COMPUTED i32 (i32.const 4))
   (global $ERROR_DETECTED i32 (i32.const 5))
   (global $loop i32 (i32.const 6))
   (global $testshift i32 (i32.const 7))
   (global $shift i32 (i32.const 8))
   (global $shift_recover i32 (i32.const 9))
   (global $reduce i32 (i32.const 10))

   (global $READ_TOKEN i32 (i32.const 0))
   (global $RAISE_PARSE_ERROR i32 (i32.const 1))
   (global $GROW_STACKS_1 i32 (i32.const 2))
   (global $GROW_STACKS_2 i32 (i32.const 3))
   (global $COMPUTE_SEMANTIC_ACTION i32 (i32.const 4))
   (global $CALL_ERROR_FUNCTION i32 (i32.const 5))

   (global $env_s_stack i32 (i32.const 1))
   (global $env_v_stack i32 (i32.const 2))
   (global $env_symb_start_stack i32 (i32.const 3))
   (global $env_symb_end_stack i32 (i32.const 4))
   (global $env_stacksize i32 (i32.const 5))
   (global $env_stackbase i32 (i32.const 6))
   (global $env_curr_char i32 (i32.const 7))
   (global $env_lval i32 (i32.const 8))
   (global $env_symb_start i32 (i32.const 9))
   (global $env_symb_end i32 (i32.const 10))
   (global $env_asp i32 (i32.const 11))
   (global $env_rule_len i32 (i32.const 12))
   (global $env_rule_number i32 (i32.const 13))
   (global $env_sp i32 (i32.const 14))
   (global $env_state i32 (i32.const 15))
   (global $env_errflag i32 (i32.const 16))

   (global $tbl_transl_const i32 (i32.const 2))
   (global $tbl_transl_block i32 (i32.const 3))
   (global $tbl_lhs i32 (i32.const 4))
   (global $tbl_len i32 (i32.const 5))
   (global $tbl_defred i32 (i32.const 6))
   (global $tbl_dgoto i32 (i32.const 7))
   (global $tbl_sindex i32 (i32.const 8))
   (global $tbl_rindex i32 (i32.const 9))
   (global $tbl_gindex i32 (i32.const 10))
   (global $tbl_tablesize i32 (i32.const 11))
   (global $tbl_table i32 (i32.const 12))
   (global $tbl_check i32 (i32.const 13))
   (global $tbl_names_const i32 (i32.const 15))
   (global $tbl_names_block i32 (i32.const 16))

   (func $strlen
(@if use-js-string
(@then
      (param $s externref)
)
(@else
      (param $s (ref $bytes))
))
      (param $p i32) (result i32)
      (local $i i32)
      (local.set $i (local.get $p))
      (loop $loop
         (if (i32.ne (call $string_get (local.get $s) (local.get $i))
               (i32.const 0))
            (then
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (i32.sub (local.get $i) (local.get $p)))

   (@string $unknown_token "<unknown token>")

   (func $token_name
      (param $vnames (ref eq)) (param $number i32) (result (ref eq))
(@if use-js-string
(@then
      (local $names externref)
)
(@else
      (local $names (ref $bytes))
))
      (local $i i32) (local $len i32)
      (local.set $names (call $string_val (local.get $vnames)))
      (loop $loop
         (if (i32.eqz (call $string_get (local.get $names) (local.get $i)))
            (then (return (global.get $unknown_token))))
         (if (i32.ne (local.get $number) (i32.const 0))
            (then
               (local.set $i
                  (i32.add (local.get $i)
                    (i32.add (call $strlen (local.get $names) (local.get $i))
                      (i32.const 1))))
               (local.set $number (i32.sub (local.get $number) (i32.const 1)))
               (br $loop))))
      (local.set $len (call $strlen (local.get $names) (local.get $i)))
      (return_call $string_sub
         (local.get $names) (local.get $i) (local.get $len)))

   (func $output (param $s (ref eq))
      (drop
         (call $caml_ml_output (global.get $caml_stderr)
            (local.get $s) (ref.i31 (i32.const 0))
            (ref.i31 (call $caml_string_length (local.get $s))))))

   (func $output_nl
      (drop
         (call $caml_ml_output (global.get $caml_stderr)
            (@string "\n")
            (ref.i31 (i32.const 0)) (ref.i31 (i32.const 1))))
      (drop (call $caml_ml_flush (global.get $caml_stderr))))

   (@string $int_format "%d")

   (func $output_int (param i32)
      (call $output
         (call $caml_format_int (@string "%d") (ref.i31 (local.get 0)))))

   (@string $State "State ")
   (@string $read_token ": read token ")

   (func $print_token
      (param $tables (ref $block)) (param $state i32) (param $tok (ref eq))
      (local $b (ref $block))
      (local $v (ref eq))
      (if (ref.test (ref i31) (local.get $tok))
         (then
            (call $output (global.get $State))
            (call $output_int (local.get $state))
            (call $output (global.get $read_token))
            (call $output
               (call $token_name
                  (array.get $block (local.get $tables)
                     (global.get $tbl_names_const))
                  (i31.get_u (ref.cast (ref i31) (local.get $tok)))))
            (call $output_nl))
         (else
            (call $output (global.get $State))
            (call $output_int (local.get $state))
            (call $output (global.get $read_token))
            (local.set $b (ref.cast (ref $block) (local.get $tok)))
            (call $output
               (call $token_name
                  (array.get $block (local.get $tables)
                     (global.get $tbl_names_block))
                  (i31.get_u
                     (ref.cast (ref i31)
                        (array.get $block (local.get $b) (i32.const 0))))))
            (call $output (@string "("))
            (local.set $v (array.get $block (local.get $b) (i32.const 1)))
            (if (ref.test (ref i31) (local.get $v))
               (then
                  (call $output_int
                     (i31.get_s (ref.cast (ref i31) (local.get $v)))))
            (else (if (ref.test (ref $bytes) (local.get $v))
               (then (call $output (local.get $v)))
            (else (if (ref.test (ref $float) (local.get $v))
               (then
                  (call $output
                     (call $caml_format_float (@string "%g") (local.get $v))))
            (else
               (call $output (@string "_"))))))))
            (call $output (@string ")"))
            (call $output_nl))))

   (@string $recovering_in_state "Recovering in state ")
   (@string $discarding_state "Discarding state ")
   (@string $no_more_states_to_discard "No more states to discard")
   (@string $discarding_last_token_read "Discarding last token read")
   (@string $shift_to_state ": shift to state ")
   (@string $reduce_by_rule ": reduce by rule ")

   (func (export "caml_parse_engine")
      (param $vtables (ref eq)) (param $venv (ref eq)) (param $vcmd (ref eq))
      (param $varg (ref eq)) (result (ref eq))
      (local $res i32) (local $n i32) (local $n1 i32) (local $n2 i32)
      (local $m i32)
      (local $state1 i32) (local $sp i32) (local $asp i32) (local $state i32)
      (local $errflag i32)
      (local $tables (ref $block)) (local $env (ref $block)) (local $cmd i32)
      (local $arg (ref $block))
(@if use-js-string
(@then
      (local $tbl_defred externref)
      (local $tbl_sindex externref)
      (local $tbl_check externref)
      (local $tbl_rindex externref)
      (local $tbl_table externref)
      (local $tbl_len externref)
      (local $tbl_lhs externref)
      (local $tbl_gindex externref)
      (local $tbl_dgoto externref)
)
(@else
      (local $tbl_defred (ref $bytes))
      (local $tbl_sindex (ref $bytes))
      (local $tbl_check (ref $bytes))
      (local $tbl_rindex (ref $bytes))
      (local $tbl_table (ref $bytes))
      (local $tbl_len (ref $bytes))
      (local $tbl_lhs (ref $bytes))
      (local $tbl_gindex (ref $bytes))
      (local $tbl_dgoto (ref $bytes))
))
      (local.set $tables (ref.cast (ref $block) (local.get $vtables)))
      (local.set $tbl_defred
         (call $string_val
            (array.get $block (local.get $tables) (global.get $tbl_defred))))
      (local.set $tbl_sindex
         (call $string_val
            (array.get $block (local.get $tables) (global.get $tbl_sindex))))
      (local.set $tbl_check
         (call $string_val
            (array.get $block (local.get $tables) (global.get $tbl_check))))
      (local.set $tbl_rindex
         (call $string_val
            (array.get $block (local.get $tables) (global.get $tbl_rindex))))
      (local.set $tbl_table
         (call $string_val
            (array.get $block (local.get $tables) (global.get $tbl_table))))
      (local.set $tbl_len
         (call $string_val
            (array.get $block (local.get $tables) (global.get $tbl_len))))
      (local.set $tbl_lhs
         (call $string_val
            (array.get $block (local.get $tables) (global.get $tbl_lhs))))
      (local.set $tbl_gindex
         (call $string_val
            (array.get $block (local.get $tables) (global.get $tbl_gindex))))
      (local.set $tbl_dgoto
         (call $string_val
            (array.get $block (local.get $tables) (global.get $tbl_dgoto))))
      (local.set $env (ref.cast (ref $block) (local.get $venv)))
      (local.set $cmd (i31.get_s (ref.cast (ref i31) (local.get $vcmd))))
      (local.set $sp
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $env) (global.get $env_sp)))))
      (local.set $state
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $env) (global.get $env_state)))))
      (local.set $errflag
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $env) (global.get $env_errflag)))))
      (block $exit
       (loop $next
        (block $default
         (block $SEMANTIC_ACTION_COMPUTED
          (block $STACKS_GROWN_2
           (block $reduce
            (block $STACKS_GROWN_1
             (block $shift_recover
              (block $shift
               (block $ERROR_DETECTED
                (block $testshift
                 (block $TOKEN_READ
                  (block $loop
                   (block $START
                    (br_table $START $TOKEN_READ $STACKS_GROWN_1 $STACKS_GROWN_2
                       $SEMANTIC_ACTION_COMPUTED $ERROR_DETECTED $loop
                       $testshift $shift $shift_recover $reduce $default
                       (local.get $cmd)))
                   ;; START:
                   (local.set $state (i32.const 0))
                   (local.set $errflag (i32.const 0)))
                   ;; Fall through
                  ;; loop:
                  (local.set $n
                     (call $get (local.get $tbl_defred) (local.get $state)))
                  (if (i32.ne (local.get $n) (i32.const 0))
                     (then
                        (local.set $cmd (global.get $reduce))
                        (br $next)))
                  (if (i32.ge_s
                         (i31.get_s
                            (ref.cast (ref i31)
                               (array.get $block (local.get $env)
                                  (global.get $env_curr_char))))
                         (i32.const 0))
                     (then
                        (local.set $cmd (global.get $testshift))
                        (br $next)))
                  (local.set $res (global.get $READ_TOKEN))
                  (br $exit))
                 ;; TOKEN_READ:
                 (block $cont
                    (drop (block $not_block (result (ref eq))
                       (local.set $arg
                          (br_on_cast_fail $not_block (ref eq) (ref $block)
                             (local.get $varg)))
                       (array.set $block (local.get $env)
                          (global.get $env_curr_char)
                          (array.get $block
                             (ref.cast (ref $block)
                                (array.get $block (local.get $tables)
                                   (global.get $tbl_transl_block)))
                             (i32.add
                                (i31.get_u
                                   (ref.cast (ref i31)
                                      (array.get $block
                                         (local.get $arg) (i32.const 0))))
                                (i32.const 1))))
                       (array.set $block (local.get $env) (global.get $env_lval)
                          (array.get $block (local.get $arg) (i32.const 1)))
                       (br $cont)))
                    (array.set $block (local.get $env)
                       (global.get $env_curr_char)
                       (array.get $block
                          (ref.cast (ref $block)
                             (array.get $block (local.get $tables)
                                (global.get $tbl_transl_const)))
                          (i32.add
                             (i31.get_u (ref.cast (ref i31) (local.get $varg)))
                             (i32.const 1))))
                    (array.set $block (local.get $env) (global.get $env_lval)
                       (ref.i31 (i32.const 0))))
                 (if (global.get $caml_parser_trace)
                    (then (call $print_token (local.get $tables)
                       (local.get $state) (local.get $varg)))))
                 ;; Fall through
                ;; testshift:
                (local.set $n1
                   (call $get (local.get $tbl_sindex) (local.get $state)))
                (local.set $n2
                    (i32.add (local.get $n1)
                       (i31.get_s
                          (ref.cast (ref i31)
                             (array.get $block (local.get $env)
                                (global.get $env_curr_char))))))
                (if (i32.and
                       (i32.ne (local.get $n1) (i32.const 0))
                       (i32.ge_s (local.get $n2) (i32.const 0)))
                   (then
                      (if (i32.le_s (local.get $n2)
                             (i31.get_s
                                (ref.cast (ref i31)
                                   (array.get $block (local.get $tables)
                                      (global.get $tbl_tablesize)))))
                         (then
                            (if (ref.eq
                                   (ref.i31
                                      (call $get (local.get $tbl_check)
                                         (local.get $n2)))
                                   (array.get $block (local.get $env)
                                      (global.get $env_curr_char)))
                               (then
                                  (local.set $cmd (global.get $shift))
                                  (br $next)))))))
                (local.set $n1
                   (call $get (local.get $tbl_rindex) (local.get $state)))
                (local.set $n2
                   (i32.add (local.get $n1)
                      (i31.get_s
                         (ref.cast (ref i31)
                            (array.get $block (local.get $env)
                               (global.get $env_curr_char))))))
                (if (i32.and
                       (i32.ne (local.get $n1) (i32.const 0))
                       (i32.ge_s (local.get $n2) (i32.const 0)))
                   (then
                      (if (i32.le_s (local.get $n2)
                             (i31.get_s
                                (ref.cast (ref i31)
                                   (array.get $block (local.get $tables)
                                      (global.get $tbl_tablesize)))))
                         (then
                            (if (ref.eq
                                   (ref.i31
                                      (call $get (local.get $tbl_check)
                                         (local.get $n2)))
                                   (array.get $block (local.get $env)
                                      (global.get $env_curr_char)))
                               (then
                                  (local.set $n
                                     (call $get (local.get $tbl_table)
                                        (local.get $n2)))
                                  (local.set $cmd (global.get $reduce))
                                  (br $next)))))))
                (if (i32.le_s (local.get $errflag) (i32.const 0))
                   (then
                      (local.set $res (global.get $CALL_ERROR_FUNCTION))
                      (br $exit))))
                ;; Fall through
               ;; ERROR_DETECTED:
               (if (i32.lt_s (local.get $errflag) (i32.const 3))
                  (then
                     (local.set $errflag (i32.const 3))
                     (loop $loop2
                       (local.set $state1
                          (i31.get_s
                             (ref.cast (ref i31)
                                (array.get $block
                                   (ref.cast (ref $block)
                                      (array.get $block (local.get $env)
                                         (global.get $env_s_stack)))
                                   (i32.add (local.get $sp) (i32.const 1))))))
                       (local.set $n1
                          (call $get (local.get $tbl_sindex)
                             (local.get $state1)))
                       (local.set $n2
                          (i32.add (local.get $n1) (global.get $ERRCODE)))
                       (if (i32.and
                              (i32.ne (local.get $n1) (i32.const 0))
                              (i32.ge_s (local.get $n2) (i32.const 0)))
                          (then
                             (if (i32.le_s (local.get $n2)
                                    (i31.get_s
                                       (ref.cast (ref i31)
                                          (array.get $block (local.get $tables)
                                             (global.get $tbl_tablesize)))))
                                (then
                                   (if (i32.eq
                                          (call $get (local.get $tbl_check)
                                             (local.get $n2))
                                          (global.get $ERRCODE))
                                      (then
                                         (if (global.get $caml_parser_trace)
                                            (then
                                               (call $output
                                                  (global.get
                                                     $recovering_in_state))
                                               (call $output_int
                                                  (local.get $state1))
                                               (call $output_nl)))
                                         (local.set $cmd
                                            (global.get $shift_recover))
                                         (br $next)))))))
                       (if (global.get $caml_parser_trace)
                          (then
                             (call $output (global.get $discarding_state))
                             (call $output_int (local.get $state1))
                             (call $output_nl)))
                       (if (i32.le_s (local.get $sp)
                              (i31.get_s
                                 (ref.cast (ref i31)
                                    (array.get $block (local.get $env)
                                       (global.get $env_stackbase)))))
                          (then
                             (if (global.get $caml_parser_trace)
                                (then
                                   (call $output
                                      (global.get $no_more_states_to_discard))
                                   (call $output_nl)))
                             (return (ref.i31 (global.get $RAISE_PARSE_ERROR)))))
                       (local.set $sp (i32.sub (local.get $sp) (i32.const 1)))
                       (br $loop2)))
                  (else
                     (if (ref.eq
                            (array.get $block (local.get $env)
                               (global.get $env_curr_char))
                            (ref.i31 (i32.const 0)))
                        (then
                           (return (ref.i31 (global.get $RAISE_PARSE_ERROR)))))
                     (if (global.get $caml_parser_trace)
                        (then
                           (call $output
                              (global.get $discarding_last_token_read))
                           (call $output_nl)))
                     (array.set $block (local.get $env)
                        (global.get $env_curr_char)
                        (ref.i31 (i32.const -1)))
                     (local.set $cmd (global.get $loop))
                     (br $next))))
              ;; shift:
              (array.set $block (local.get $env) (global.get $env_curr_char)
                 (ref.i31 (i32.const -1)))
              (if (i32.gt_s (local.get $errflag) (i32.const 0))
                 (then
                    (local.set $errflag
                       (i32.sub (local.get $errflag) (i32.const 1))))))
              ;; Fall through
             ;; shift_recover:
             (if (global.get $caml_parser_trace)
                (then
                   (call $output (global.get $State))
                   (call $output_int (local.get $state))
                   (call $output (global.get $shift_to_state))
                   (call $output_int
                      (call $get (local.get $tbl_table) (local.get $n2)))
                   (call $output_nl)))
             (local.set $state
                (call $get (local.get $tbl_table) (local.get $n2)))
             (local.set $sp (i32.add (local.get $sp) (i32.const 1)))
             (if (i32.ge_s (local.get $sp)
                    (i31.get_s
                       (ref.cast (ref i31)
                           (array.get $block (local.get $env)
                              (global.get $env_stacksize)))))
                (then
                   (local.set $res (global.get $GROW_STACKS_1))
                   (br $exit))))
             ;; Fall through
            ;; STACKS_GROWN_1:
            (array.set $block
               (ref.cast (ref $block)
                  (array.get $block (local.get $env) (global.get $env_s_stack)))
               (i32.add (local.get $sp) (i32.const 1))
               (ref.i31 (local.get $state)))
            (array.set $block
               (ref.cast (ref $block)
                  (array.get $block (local.get $env) (global.get $env_v_stack)))
               (i32.add (local.get $sp) (i32.const 1))
               (array.get $block (local.get $env) (global.get $env_lval)))
            (array.set $block
               (ref.cast (ref $block)
                  (array.get $block (local.get $env)
                     (global.get $env_symb_start_stack)))
               (i32.add (local.get $sp) (i32.const 1))
               (array.get $block (local.get $env) (global.get $env_symb_start)))
            (array.set $block
               (ref.cast (ref $block)
                  (array.get $block (local.get $env)
                     (global.get $env_symb_end_stack)))
               (i32.add (local.get $sp) (i32.const 1))
               (array.get $block (local.get $env) (global.get $env_symb_end)))
            (local.set $cmd (global.get $loop))
            (br $next))
           ;; reduce:
           (if (global.get $caml_parser_trace)
              (then
                 (call $output (global.get $State))
                 (call $output_int (local.get $state))
                 (call $output (global.get $reduce_by_rule))
                 (call $output_int (local.get $n))
                 (call $output_nl)))
           (local.set $m (call $get (local.get $tbl_len) (local.get $n)))
           (array.set $block (local.get $env) (global.get $env_asp)
              (ref.i31 (local.get $sp)))
           (array.set $block (local.get $env) (global.get $env_rule_number)
              (ref.i31 (local.get $n)))
           (array.set $block (local.get $env) (global.get $env_rule_len)
              (ref.i31 (local.get $m)))
           (local.set $sp
              (i32.add (local.get $sp) (i32.sub (i32.const 1) (local.get $m))))
           (local.set $m (call $get (local.get $tbl_lhs) (local.get $n)))
           (local.set $state1
              (i31.get_s
                 (ref.cast (ref i31)
                    (array.get $block
                       (ref.cast (ref $block)
                          (array.get $block (local.get $env)
                             (global.get $env_s_stack)))
                       (local.get $sp)))))
           (local.set $n1 (call $get (local.get $tbl_gindex) (local.get $m)))
           (local.set $n2 (i32.add (local.get $n1) (local.get $state1)))
           (block $cont
              (if (i32.and
                     (i32.ne (local.get $n1) (i32.const 0))
                     (i32.ge_s (local.get $n2) (i32.const 0)))
                 (then
                    (if (i32.le_s (local.get $n2)
                           (i31.get_s
                              (ref.cast (ref i31)
                                 (array.get $block (local.get $tables)
                                    (global.get $tbl_tablesize)))))
                       (then
                          (if (i32.eq
                                 (call $get (local.get $tbl_check)
                                    (local.get $n2))
                                 (local.get $state1))
                             (then
                                (local.set $state
                                   (call $get (local.get $tbl_table)
                                      (local.get $n2)))
                                (br $cont)))))))
              (local.set $state
                 (call $get (local.get $tbl_dgoto) (local.get $m))))
           (if (i32.ge_s (local.get $sp)
                  (i31.get_s
                     (ref.cast (ref i31)
                        (array.get $block (local.get $env)
                           (global.get $env_stacksize)))))
              (then
                 (local.set $res (global.get $GROW_STACKS_2))
                 (br $exit))))
           ;; Fall through
          ;; STACKS_GROWN_2:
          (local.set $res (global.get $COMPUTE_SEMANTIC_ACTION))
          (br $exit))
         ;; SEMANTIC_ACTION_COMPUTED:
         (array.set $block
            (ref.cast (ref $block)
               (array.get $block (local.get $env) (global.get $env_s_stack)))
            (i32.add (local.get $sp) (i32.const 1))
            (ref.i31 (local.get $state)))
         (array.set $block
            (ref.cast (ref $block)
               (array.get $block (local.get $env) (global.get $env_v_stack)))
            (i32.add (local.get $sp) (i32.const 1))
            (local.get $varg))
         (local.set $asp
            (i31.get_s
               (ref.cast (ref i31)
                  (array.get $block (local.get $env) (global.get $env_asp)))))
         (array.set $block
            (ref.cast (ref $block)
               (array.get $block (local.get $env)
                  (global.get $env_symb_end_stack)))
            (i32.add (local.get $sp) (i32.const 1))
            (array.get $block
               (ref.cast (ref $block)
                  (array.get $block (local.get $env)
                     (global.get $env_symb_end_stack)))
               (i32.add (local.get $asp) (i32.const 1))))
         (if (i32.gt_s (local.get $sp) (local.get $asp))
            (then
            ;; This is an epsilon production. Take symb_start equal to symb_end.
               (array.set $block
                  (ref.cast (ref $block)
                     (array.get $block (local.get $env)
                        (global.get $env_symb_start_stack)))
                  (i32.add (local.get $sp) (i32.const 1))
                  (array.get $block
                     (ref.cast (ref $block)
                        (array.get $block (local.get $env)
                           (global.get $env_symb_end_stack)))
                     (i32.add (local.get $asp) (i32.const 1))))))
         (local.set $cmd (global.get $loop))
         (br $next))
        ;; default:
        (return (ref.i31 (global.get $RAISE_PARSE_ERROR)))))
      ;; SAVE
      (array.set $block (local.get $env) (global.get $env_sp)
         (ref.i31 (local.get $sp)))
      (array.set $block (local.get $env) (global.get $env_state)
         (ref.i31 (local.get $state)))
      (array.set $block (local.get $env) (global.get $env_errflag)
         (ref.i31 (local.get $errflag)))
      (ref.i31 (local.get $res)))

   (func (export "caml_set_parser_trace") (param (ref eq)) (result (ref eq))
      (local $oldflag i32)
      (local.set $oldflag (global.get $caml_parser_trace))
      (global.set $caml_parser_trace
         (i31.get_s (ref.cast (ref i31) (local.get 0))))
      (ref.i31 (local.get $oldflag)))
)
