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
   (import "fail" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "string" "caml_string_of_bytes"
      (func $caml_string_of_bytes (param (ref eq)) (result (ref eq))))
   (import "string" "caml_blit_string"
      (func $caml_blit_string
         (param (ref eq) (ref eq) (ref eq) (ref eq) (ref eq))
         (result (ref eq))))

(@if use-js-string
(@then
   (import "wasm:js-string" "length"
      (func $string_length (param externref) (result i32)))
   (import "wasm:js-string" "charCodeAt"
      (func $string_get (param externref i32) (result i32)))

   (func $string_val (param $s (ref eq)) (result externref)
      (extern.convert_any
         (struct.get $string 0 (ref.cast (ref $string) (local.get $s)))))
)
(@else
   (func $string_length (param $s (ref $bytes)) (result i32)
      (array.len (local.get $s)))
   (func $string_get (param $s (ref $bytes)) (param $i i32) (result i32)
      (array.get $bytes (local.get $s) (local.get $i)))
   (func $string_val (param $s (ref eq)) (result (ref $bytes))
      (ref.cast (ref $bytes) (local.get $s)))
))

   (type $bytes (array (mut i8)))
   (type $string (struct (field anyref)))
   (type $block (array (mut (ref eq))))

   (type $char_table (array i8))
   (type $int_array (array (mut i32)))

   (global $re_word_letters (ref $char_table)
      (array.new_fixed $char_table 32
         (i32.const 0x00) (i32.const 0x00)
         (i32.const 0x00) (i32.const 0x00)   ;; 0x00-0x1F: none
         (i32.const 0x00) (i32.const 0x00)
         (i32.const 0xFF) (i32.const 0x03)   ;; 0x20-0x3F: digits 0-9
         (i32.const 0xFE) (i32.const 0xFF)
         (i32.const 0xFF) (i32.const 0x87)   ;; 0x40-0x5F: A to Z, _
         (i32.const 0xFE) (i32.const 0xFF)
         (i32.const 0xFF) (i32.const 0x07)   ;; 0x60-0x7F: a to z
         (i32.const 0x00) (i32.const 0x00)
         (i32.const 0x00) (i32.const 0x00)   ;; 0x80-0x9F: none
         (i32.const 0x00) (i32.const 0x00)
         (i32.const 0x00) (i32.const 0x00)   ;; 0xA0-0xBF: none
         (i32.const 0xFF) (i32.const 0xFF)   ;; 0xC0-0xDF:
         (i32.const 0x7F) (i32.const 0xFF)   ;; Latin-1 accented uppercase
         (i32.const 0xFF) (i32.const 0xFF)   ;; 0xE0-0xFF:
         (i32.const 0x7F) (i32.const 0xFF))) ;; Latin-1 accented lowercase

   (type $stack (sub (struct (field (ref null $stack)))))
   (type $pos
      (sub final $stack
         (struct
            (field $pos_previous (ref null $stack))
            (field $pc i32)
            (field $pos i32))))
   (type $undo
      (sub final $stack
         (struct
            (field $undo_previous (ref null $stack))
            (field $tbl (ref $int_array))
            (field $idx i32)
            (field $val i32))))

   (func $is_word_letter (param $c i32) (result i32)
      (i32.and (i32.const 1)
         (i32.shr_u
            (array.get_u $char_table (global.get $re_word_letters)
               (i32.shr_u (local.get $c) (i32.const 3)))
            (i32.and (local.get $c) (i32.const 7)))))

   (func $in_bitset
(@if use-js-string
(@then
      (param $s externref)
)
(@else
      (param $s (ref $bytes))
))
      (param $c i32) (result i32)
      (i32.and (i32.const 1)
         (i32.shr_u
            (call $string_get (local.get $s)
               (i32.shr_u (local.get $c) (i32.const 3)))
            (i32.and (local.get $c) (i32.const 7)))))

   (func $re_match
      (param $vre (ref eq))
(@if use-js-string
(@then
      (param $s externref)
)
(@else
      (param $s (ref $bytes))
))
      (param $pos i32) (param $accept_partial_match i32) (result (ref eq))
      (local $res (ref $block))
(@if use-js-string
(@then
      (local $s' externref)
      (local $set externref)
      (local $normtable externref)
)
(@else
      (local $s' (ref $bytes))
      (local $set (ref $bytes))
      (local $normtable (ref $bytes))
))
      (local $len i32) (local $instr i32) (local $arg i32) (local $i i32)
      (local $j i32) (local $l i32)
      (local $re (ref $block))
      (local $prog (ref $block))
      (local $cpool (ref $block))
      (local $numgroups i32)
      (local $numregisters i32)
      (local $group_start (ref $int_array))
      (local $group_end (ref $int_array))
      (local $re_register (ref $int_array))
      (local $pc i32)
      (local $stack (ref null $stack))
      (local $u (ref $undo))
      (local $p (ref $pos))
      (local.set $len (call $string_length (local.get $s)))
      (local.set $re (ref.cast (ref $block) (local.get $vre)))
      (local.set $prog
         (ref.cast (ref $block)
            (array.get $block (local.get $re) (i32.const 1))))
      (local.set $cpool
         (ref.cast (ref $block)
            (array.get $block (local.get $re) (i32.const 2))))
      (local.set $normtable
         (call $string_val (array.get $block (local.get $re) (i32.const 3))))
      (local.set $numgroups
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $re) (i32.const 4)))))
      (local.set $numregisters
         (i31.get_s
            (ref.cast (ref i31)
               (array.get $block (local.get $re) (i32.const 5)))))
      (local.set $group_start
         (array.new $int_array (i32.const -1) (local.get $numgroups)))
      (local.set $group_end
         (array.new $int_array (i32.const -1) (local.get $numgroups)))
      (local.set $re_register
         (array.new $int_array (i32.const -1) (local.get $numregisters)))
      (local.set $pc (i32.const 1))
      (array.set $int_array (local.get $group_start) (i32.const 0)
         (local.get $pos))
      (block $reject
       (block $ACCEPT
        (loop $continue
         (block $backtrack
          (block $prefix_match
           (block $CHECKPROGRESS
            (block $SETMARK
             (block $PUSHBACK
              (block $GOTO
               (block $SIMPLEPLUS
                (block $SIMPLESTAR
                 (block $SIMPLEOPT
                  (block $REFGROUP
                   (block $ENDGROUP
                    (block $BEGGROUP
                     (block $WORDBOUNDARY
                      (block $EOL
                       (block $BOL
                        (block $CHARCLASS
                         (block $STRINGNORM
                          (block $STRING
                           (block $CHARNORM
                            (block $CHAR
                             (local.set $instr
                                (i31.get_s
                                   (ref.cast (ref i31)
                                      (array.get $block (local.get $prog)
                                         (local.get $pc)))))
                             (local.set $pc
                                (i32.add (local.get $pc) (i32.const 1)))
                             (br_table
                                $CHAR $CHARNORM $STRING $STRINGNORM $CHARCLASS
                                $BOL $EOL $WORDBOUNDARY $BEGGROUP $ENDGROUP
                                $REFGROUP $ACCEPT $SIMPLEOPT $SIMPLESTAR
                                $SIMPLEPLUS $GOTO $PUSHBACK $SETMARK
                                $CHECKPROGRESS
                                (i32.and (local.get $instr) (i32.const 0xff))))
                            ;; CHAR
                            (br_if $prefix_match
                               (i32.eq (local.get $pos) (local.get $len)))
                            (local.set $arg
                               (i32.shr_u (local.get $instr) (i32.const 8)))
                            (br_if $backtrack
                               (i32.ne (local.get $arg)
                                  (call $string_get
                                     (local.get $s) (local.get $pos))))
                            (local.set $pos
                               (i32.add (local.get $pos) (i32.const 1)))
                            (br $continue))
                           ;; CHARNORM
                           (br_if $prefix_match
                              (i32.eq (local.get $pos) (local.get $len)))
                           (local.set $arg
                              (i32.shr_u (local.get $instr) (i32.const 8)))
                           (br_if $backtrack
                              (i32.ne (local.get $arg)
                                 (call $string_get
                                    (local.get $normtable)
                                    (call $string_get
                                       (local.get $s) (local.get $pos)))))
                           (local.set $pos
                              (i32.add (local.get $pos) (i32.const 1)))
                           (br $continue))
                          ;; STRING
                          (local.set $arg
                             (i32.shr_u (local.get $instr) (i32.const 8)))
                          (local.set $s'
                             (call $string_val
                                (array.get $block (local.get $cpool)
                                   (i32.add (local.get $arg)
                                      (i32.const 1)))))
                          (local.set $i (i32.const 0))
                          (local.set $l (call $string_length (local.get $s')))
                          (loop $loop
                             (if (i32.lt_u (local.get $i) (local.get $l))
                                (then
                                   (br_if $prefix_match
                                      (i32.eq
                                         (local.get $pos) (local.get $len)))
                                   (br_if $backtrack
                                      (i32.ne
                                         (call $string_get (local.get $s')
                                            (local.get $i))
                                         (call $string_get (local.get $s)
                                            (local.get $pos))))
                                   (local.set $pos
                                      (i32.add (local.get $pos) (i32.const 1)))
                                   (local.set $i
                                      (i32.add (local.get $i) (i32.const 1)))
                                   (br $loop))))
                          (br $continue))
                         ;; STRINGNORM
                         (local.set $arg
                            (i32.shr_u (local.get $instr) (i32.const 8)))
                         (local.set $s'
                            (call $string_val
                               (array.get $block (local.get $cpool)
                                  (i32.add (local.get $arg)
                                      (i32.const 1)))))
                         (local.set $i (i32.const 0))
                         (local.set $l (call $string_length (local.get $s')))
                         (loop $loop
                            (if (i32.lt_u (local.get $i) (local.get $l))
                               (then
                                  (br_if $prefix_match
                                     (i32.eq
                                        (local.get $pos) (local.get $len)))
                                  (br_if $backtrack
                                     (i32.ne
                                        (call $string_get (local.get $s')
                                           (local.get $i))
                                        (call $string_get
                                           (local.get $normtable)
                                           (call $string_get (local.get $s)
                                              (local.get $pos)))))
                                  (local.set $pos
                                     (i32.add (local.get $pos) (i32.const 1)))
                                  (local.set $i
                                     (i32.add (local.get $i) (i32.const 1)))
                                  (br $loop))))
                         (br $continue))
                        ;; CHARCLASS
                        (br_if $prefix_match
                           (i32.eq (local.get $pos) (local.get $len)))
                        (local.set $arg
                           (i32.shr_u (local.get $instr) (i32.const 8)))
                        (br_if $backtrack
                           (i32.eqz
                              (call $in_bitset
                                 (call $string_val
                                    (array.get $block (local.get $cpool)
                                       (i32.add (local.get $arg)
                                          (i32.const 1))))
                                 (call $string_get (local.get $s)
                                    (local.get $pos)))))
                        (local.set $pos
                           (i32.add (local.get $pos) (i32.const 1)))
                        (br $continue))
                       ;; BOL
                       (br_if $continue (i32.eqz (local.get $pos)))
                       (br_if $continue
                          (i32.eq (@char "\n")
                             (call $string_get (local.get $s)
                                (i32.sub (local.get $pos) (i32.const 1)))))
                       (br $backtrack))
                      ;; EOL
                      (br_if $continue
                         (i32.eq (local.get $pos) (local.get $len)))
                      (br_if $continue
                         (i32.eq (@char "\n")
                            (call $string_get (local.get $s)
                               (local.get $pos))))
                      (br $backtrack))
                     ;; WORDBOUNDARY
                     (if (i32.eqz (local.get $pos))
                        (then
                           (br_if $prefix_match
                              (i32.eq (local.get $pos) (local.get $len)))
                           (br_if $continue
                              (call $is_word_letter
                                 (call $string_get (local.get $s)
                                    (local.get $pos))))
                           (br $backtrack))
                        (else
                           (if (i32.eq (local.get $pos) (local.get $len))
                              (then
                                 (br_if $continue
                                    (call $is_word_letter
                                       (call $string_get (local.get $s)
                                          (i32.sub (local.get $pos)
                                             (i32.const 1)))))
                                 (br $backtrack))
                              (else
                                 (br_if $continue
                                    (i32.ne
                                       (call $is_word_letter
                                          (call $string_get (local.get $s)
                                             (i32.sub (local.get $pos)
                                                (i32.const 1))))
                                       (call $is_word_letter
                                          (call $string_get (local.get $s)
                                             (local.get $pos)))))
                                 (br $backtrack))))))
                    ;; BEGGROUP
                    (local.set $arg
                       (i32.shr_u (local.get $instr) (i32.const 8)))
                    (local.set $stack
                       (struct.new $undo
                          (local.get $stack)
                          (local.get $group_start)
                          (local.get $arg)
                          (array.get $int_array
                             (local.get $group_start) (local.get $arg))))
                    (array.set $int_array (local.get $group_start)
                       (local.get $arg) (local.get $pos))
                    (br $continue))
                   ;; ENDGROUP
                   (local.set $arg
                      (i32.shr_u (local.get $instr) (i32.const 8)))
                    (local.set $stack
                       (struct.new $undo
                          (local.get $stack)
                          (local.get $group_end)
                          (local.get $arg)
                          (array.get $int_array
                             (local.get $group_end) (local.get $arg))))
                   (array.set $int_array (local.get $group_end)
                      (local.get $arg) (local.get $pos))
                   (br $continue))
                  ;; REFGROUP
                  (local.set $arg
                     (i32.shr_u (local.get $instr) (i32.const 8)))
                  (local.set $i
                     (array.get $int_array (local.get $group_start)
                        (local.get $arg)))
                  (local.set $j
                     (array.get $int_array (local.get $group_end)
                        (local.get $arg)))
                  (br_if $backtrack
                     (i32.or (i32.lt_s (local.get $i) (i32.const 0))
                         (i32.lt_s (local.get $j) (i32.const 0))))
                  (loop $loop
                     (if (i32.lt_u (local.get $i) (local.get $j))
                        (then
                           (br_if $prefix_match
                              (i32.eq (local.get $pos) (local.get $len)))
                           (br_if $backtrack
                              (i32.ne
                                 (call $string_get (local.get $s)
                                    (local.get $i))
                                 (call $string_get (local.get $s)
                                    (local.get $pos))))
                           (local.set $pos
                              (i32.add (local.get $pos) (i32.const 1)))
                           (local.set $i
                              (i32.add (local.get $i) (i32.const 1)))
                           (br $loop))))
                  (br $continue))
                 ;; SIMPLEOPT
                 (local.set $arg (i32.shr_u (local.get $instr) (i32.const 8)))
                 (if (i32.lt_u (local.get $pos) (local.get $len))
                    (then
                       (if (call $in_bitset
                              (call $string_val
                                 (array.get $block (local.get $cpool)
                                    (i32.add (local.get $arg)
                                       (i32.const 1))))
                              (call $string_get (local.get $s)
                                 (local.get $pos)))
                          (then
                             (local.set $pos
                                (i32.add (local.get $pos) (i32.const 1)))))))
                 (br $continue))
                ;; SIMPLESTAR
                (local.set $arg (i32.shr_u (local.get $instr) (i32.const 8)))
                (local.set $set
                   (call $string_val
                      (array.get $block (local.get $cpool)
                         (i32.add (local.get $arg) (i32.const 1)))))
                (loop $loop
                   (if (i32.lt_u (local.get $pos) (local.get $len))
                      (then
                         (if (call $in_bitset (local.get $set)
                                (call $string_get (local.get $s)
                                   (local.get $pos)))
                            (then
                               (local.set $pos
                                  (i32.add (local.get $pos) (i32.const 1)))
                               (br $loop))))))
                (br $continue))
               ;; SIMPLEPLUS
               (br_if $prefix_match (i32.eq (local.get $pos) (local.get $len)))
               (local.set $arg (i32.shr_u (local.get $instr) (i32.const 8)))
               (local.set $set
                  (call $string_val
                     (array.get $block (local.get $cpool)
                        (i32.add (local.get $arg) (i32.const 1)))))
               (br_if $backtrack
                  (i32.eqz
                     (call $in_bitset (local.get $set)
                        (call $string_get (local.get $s) (local.get $pos)))))
               (loop $loop
                  (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
                  (if (i32.lt_u (local.get $pos) (local.get $len))
                     (then
                        (br_if $loop
                           (call $in_bitset (local.get $set)
                              (call $string_get (local.get $s)
                                 (local.get $pos)))))))
               (br $continue))
              ;; GOTO
              (local.set $pc
                 (i32.add
                    (local.get $pc)
                    (i32.shr_s (local.get $instr) (i32.const 8))))
              (br $continue))
             ;; PUSHBACK
             (local.set $stack
                (struct.new $pos
                   (local.get $stack)
                   (i32.add (local.get $pc)
                      (i32.shr_s (local.get $instr) (i32.const 8)))
                   (local.get $pos)))
             (br $continue))
            ;; SETMARK
            (local.set $arg (i32.shr_u (local.get $instr) (i32.const 8)))
            (local.set $stack
               (struct.new $undo
                  (local.get $stack)
                  (local.get $re_register)
                  (local.get $arg)
                  (array.get $int_array
                     (local.get $re_register) (local.get $arg))))
            (array.set $int_array (local.get $re_register) (local.get $arg)
               (local.get $pos))
            (br $continue))
           ;; CHECKPROGRESS
           (local.set $arg (i32.shr_u (local.get $instr) (i32.const 8)))
           (br_if $backtrack
              (i32.eq (local.get $pos)
                 (array.get $int_array (local.get $re_register)
                    (local.get $arg))))
           (br $continue))
          ;; prefix_match
          (br_if $ACCEPT (local.get $accept_partial_match)))
         ;; backtrack
         (loop $loop
            (local.set $u
               (ref.cast (ref $undo)
                  (block $undo (result (ref $stack))
                     (local.set $p
                        (br_on_cast_fail $undo (ref eq) (ref $pos)
                           (br_on_null $reject (local.get $stack))))
                     (local.set $pc (struct.get $pos $pc (local.get $p)))
                     (local.set $pos (struct.get $pos $pos (local.get $p)))
                     (local.set $stack
                        (struct.get $pos $pos_previous (local.get $p)))
                     (br $continue))))
            (array.set $int_array (struct.get $undo $tbl (local.get $u))
                (struct.get $undo $idx (local.get $u))
                (struct.get $undo $val (local.get $u)))
            (local.set $stack (struct.get $undo $undo_previous (local.get $u)))
            (br $loop))))
       ;; ACCEPT
       (array.set $int_array
          (local.get $group_end) (i32.const 0) (local.get $pos))
       (local.set $res
          (array.new $block (ref.i31 (i32.const 0))
             (i32.add (i32.shl (local.get $numgroups) (i32.const 1))
                (i32.const 1))))
       (local.set $i (i32.const 0))
       (loop $loop
          (if (i32.lt_u (local.get $i) (local.get $numgroups))
             (then
                (local.set $j (i32.shl (local.get $i) (i32.const 1)))
                (if (i32.or
                       (i32.lt_s
                          (array.get $int_array (local.get $group_start)
                             (local.get $i))
                          (i32.const 0))
                       (i32.lt_s
                          (array.get $int_array (local.get $group_end)
                             (local.get $i))
                          (i32.const 0)))
                   (then
                      (array.set $block (local.get $res)
                         (i32.add (local.get $j) (i32.const 1))
                         (ref.i31 (i32.const -1)))
                      (array.set $block (local.get $res)
                         (i32.add (local.get $j) (i32.const 2))
                         (ref.i31 (i32.const -1))))
                   (else
                      (array.set $block (local.get $res)
                         (i32.add (local.get $j) (i32.const 1))
                         (ref.i31
                            (array.get $int_array (local.get $group_start)
                               (local.get $i))))
                      (array.set $block (local.get $res)
                         (i32.add (local.get $j) (i32.const 2))
                         (ref.i31
                            (array.get $int_array (local.get $group_end)
                               (local.get $i))))))
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $loop))))
       (return (local.get $res)))
      ;; reject
      (ref.i31 (i32.const 0)))

   (@string $search_forward "Str.search_forward")

   (func (export "re_search_forward")
      (param $re (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (result (ref eq))
      ;; ZZZ startchars
(@if use-js-string
(@then
      (local $s externref)
)
(@else
      (local $s (ref $bytes))
))
      (local $pos i32) (local $len i32)
      (local $res (ref eq))
      (local.set $s (call $string_val (local.get $vs)))
      (local.set $pos (i31.get_s (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (call $string_length (local.get $s)))
      (if (i32.gt_u (local.get $pos) (local.get $len))
         (then (call $caml_invalid_argument (global.get $search_forward))))
      (loop $loop
         (local.set $res
            (call $re_match
               (local.get $re) (local.get $s) (local.get $pos) (i32.const 0)))
         (if (ref.test (ref $block) (local.get $res))
            (then
               (return (local.get $res))))
         (local.set $pos (i32.add (local.get $pos) (i32.const 1)))
         (br_if $loop (i32.le_u (local.get $pos) (local.get $len))))
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (@string $search_backward "Str.search_backward")

   (func (export "re_search_backward")
      (param $re (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (result (ref eq))
      ;; ZZZ startchars
(@if use-js-string
(@then
      (local $s externref)
)
(@else
      (local $s (ref $bytes))
))
      (local $pos i32) (local $len i32)
      (local $res (ref eq))
      (local.set $s (call $string_val (local.get $vs)))
      (local.set $pos (i31.get_s (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (call $string_length (local.get $s)))
      (if (i32.gt_u (local.get $pos) (local.get $len))
         (then (call $caml_invalid_argument (global.get $search_backward))))
      (loop $loop
         (local.set $res
            (call $re_match
               (local.get $re) (local.get $s) (local.get $pos) (i32.const 0)))
         (if (ref.test (ref $block) (local.get $res))
            (then
               (return (local.get $res))))
         (local.set $pos (i32.sub (local.get $pos) (i32.const 1)))
         (br_if $loop (i32.ge_s (local.get $pos) (i32.const 0))))
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (@string $string_match "Str.string_match")

   (func (export "re_string_match")
      (param $re (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (result (ref eq))
      ;; ZZZ startchars
(@if use-js-string
(@then
      (local $s externref)
)
(@else
      (local $s (ref $bytes))
))
      (local $pos i32) (local $len i32)
      (local $res (ref eq))
      (local.set $s (call $string_val (local.get $vs)))
      (local.set $pos (i31.get_s (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (call $string_length (local.get $s)))
      (if (i32.gt_u (local.get $pos) (local.get $len))
         (then (call $caml_invalid_argument (global.get $string_match))))
      (local.set $res
         (call $re_match
            (local.get $re) (local.get $s) (local.get $pos) (i32.const 0)))
       (if (ref.test (ref $block) (local.get $res))
          (then
            (return (local.get $res))))
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (@string $string_partial_match "Str.string_partial_match")

   (func (export "re_partial_match")
      (param $re (ref eq)) (param $vs (ref eq)) (param $vpos (ref eq))
      (result (ref eq))
      ;; ZZZ startchars
(@if use-js-string
(@then
      (local $s externref)
)
(@else
      (local $s (ref $bytes))
))
      (local $pos i32) (local $len i32)
      (local $res (ref eq))
      (local.set $s (call $string_val (local.get $vs)))
      (local.set $pos (i31.get_s (ref.cast (ref i31) (local.get $vpos))))
      (local.set $len (call $string_length (local.get $s)))
      (if (i32.gt_u (local.get $pos) (local.get $len))
         (then (call $caml_invalid_argument (global.get $string_partial_match))))
      (local.set $res
         (call $re_match
            (local.get $re) (local.get $s) (local.get $pos) (i32.const 1)))
       (if (ref.test (ref $block) (local.get $res))
          (then
            (return (local.get $res))))
      (array.new_fixed $block 1 (ref.i31 (i32.const 0))))

   (@string $illegal_backslash "Str.replace: illegal backslash sequence")
   (@string $unmatched_group "Str.replace: reference to unmatched group")

   (func (export "re_replacement_text")
      (param $vrepl (ref eq)) (param $vgroups (ref eq)) (param $orig (ref eq))
      (result (ref eq))
(@if use-js-string
(@then
      (local $repl externref)
)
(@else
      (local $repl (ref $bytes))
))
      (local $groups (ref $block))
      (local $res (ref $bytes))
      (local $i i32) (local $j i32) (local $l i32) (local $len i32)
      (local $c i32) (local $start i32) (local $end i32)
      (local.set $repl (call $string_val (local.get $vrepl)))
      (local.set $l (call $string_length (local.get $repl)))
      (local.set $groups (ref.cast (ref $block) (local.get $vgroups)))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (local.set $c
                  (call $string_get (local.get $repl) (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (if (i32.ne (local.get $c) (@char "\\"))
                  (then
                     (local.set $len (i32.add (local.get $len) (i32.const 1)))
                     (br $loop)))
               (if (i32.eq (local.get $i) (local.get $l))
                  (then (call $caml_failwith (global.get $illegal_backslash))))
               (local.set $c
                  (call $string_get (local.get $repl) (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (if (i32.eq (local.get $c) (@char "\\"))
                  (then
                     (local.set $len (i32.add (local.get $len) (i32.const 1)))
                     (br $loop)))
               (local.set $c (i32.sub (local.get $c) (@char "0")))
               (if (i32.gt_u (local.get $c) (i32.const 9))
                  (then
                     (local.set $len (i32.add (local.get $len) (i32.const 2)))
                     (br $loop)))
               (local.set $c (i32.shl (local.get $c) (i32.const 1)))
               (if (i32.gt_u (i32.add (local.get $c) (i32.const 1))
                      (array.len (local.get $groups)))
                  (then (call $caml_failwith (global.get $unmatched_group))))
               (local.set $start
                  (i31.get_s
                     (ref.cast (ref i31)
                        (array.get $block (local.get $groups)
                           (i32.add (local.get $c) (i32.const 1))))))
               (local.set $end
                  (i31.get_s
                     (ref.cast (ref i31)
                        (array.get $block (local.get $groups)
                           (i32.add (local.get $c) (i32.const 2))))))
               (if (i32.eq (local.get $start) (i32.const -1))
                  (then (call $caml_failwith (global.get $unmatched_group))))
               (local.set $len
                   (i32.add (local.get $len)
                      (i32.sub (local.get $end) (local.get $start))))
               (br $loop))))
      (local.set $res (array.new $bytes (i32.const 0) (local.get $len)))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $l))
            (then
               (local.set $c
                  (call $string_get (local.get $repl) (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (if (i32.ne (local.get $c) (@char "\\"))
                  (then
                     (array.set $bytes (local.get $res) (local.get $j)
                        (local.get $c))
                     (local.set $j (i32.add (local.get $j) (i32.const 1)))
                     (br $loop)))
               (local.set $c
                  (call $string_get (local.get $repl) (local.get $i)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (if (i32.eq (local.get $c) (@char "\\"))
                  (then
                     (array.set $bytes (local.get $res) (local.get $j)
                        (local.get $c))
                     (local.set $j (i32.add (local.get $j) (i32.const 1)))
                     (br $loop)))
               (local.set $c (i32.sub (local.get $c) (@char "0")))
               (if (i32.gt_u (local.get $c) (i32.const 9))
                  (then
                     (array.set $bytes (local.get $res) (local.get $j)
                        (@char "\\"))
                     (array.set $bytes (local.get $res)
                        (i32.add (local.get $j) (i32.const 1))
                        (i32.add (local.get $c) (@char "0")))
                     (local.set $j (i32.add (local.get $j) (i32.const 2)))
                     (br $loop)))
               (local.set $c (i32.shl (local.get $c) (i32.const 1)))
               (if (i32.gt_u (i32.add (local.get $c) (i32.const 1))
                      (array.len (local.get $groups)))
                  (then (call $caml_failwith (global.get $unmatched_group))))
               (local.set $start
                  (i31.get_s
                     (ref.cast (ref i31)
                        (array.get $block (local.get $groups)
                           (i32.add (local.get $c) (i32.const 1))))))
               (local.set $end
                  (i31.get_s
                     (ref.cast (ref i31)
                        (array.get $block (local.get $groups)
                           (i32.add (local.get $c) (i32.const 2))))))
               (local.set $len (i32.sub (local.get $end) (local.get $start)))
               (drop
                  (call $caml_blit_string
                     (local.get $orig) (ref.i31 (local.get $start))
                     (local.get $res) (ref.i31 (local.get $j))
                     (ref.i31 (local.get $len))))
               (local.set $j (i32.add (local.get $j) (local.get $len)))
               (br $loop))))
      (call $caml_string_of_bytes (local.get $res)))
)
