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
(@if (>= $ocaml_version (5 1 0))
(@then
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "marshal" "caml_intern_decompress_input"
      (global $caml_intern_decompress_input (mut (ref null $decompress))))
   (import "zstd" "zstd_memory" (memory 2))
   (import "zstd" "zstd_reset" (func $zstd_reset))
   (import "zstd" "zstd_alloc" (func $zstd_alloc (param i32) (result i32)))
   (import "zstd" "zstd_decompress"
      (func $zstd_decompress (param i32 i32 i32 i32) (result i32)))
   (import "zstd" "zstd_window_size"
      (func $window_size (param i32 i32) (result i32)))
   (import "zstd" "zstd_in_buf" (func $in_buf (result i32)))
   (import "zstd" "zstd_in_cap" (func $in_cap (result i32)))
   (import "zstd" "zstd_out_buf" (func $out_buf (result i32)))
   (import "zstd" "zstd_stream_init" (func $stream_init (result i32)))
   (import "zstd" "zstd_stream_set_input" (func $stream_set_input (param i32)))
   (import "zstd" "zstd_stream_run" (func $stream_run (result i32)))
   (import "zstd" "zstd_out_produced" (func $out_produced (result i32)))
   (import "zstd" "zstd_in_done" (func $in_done (result i32)))

   (type $bytes (array (mut i8)))
   (type $decompress
      (func (param (ref $bytes) i32 i32 i32) (result (ref $bytes))))

   (@string $decompress_failed "input_value: zstd decompression failed")

   ;; Bytes needed to parse any zstd frame header (= ZSTD_FRAMEHEADERSIZE_MAX).
   (global $frame_header_max i32 (i32.const 18))

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

   ;; One-shot decode of a small frame: blit the whole compressed input into
   ;; linear memory, decode straight into an out_len-sized buffer (which zstd
   ;; reuses as its history window), then blit the result into a GC array.
   (func $decompress_oneshot
      (param $input (ref $bytes)) (param $pos i32) (param $len i32)
      (param $out_len i32) (result (ref $bytes))
      (local $in_buf i32) (local $out_buf i32) (local $output (ref $bytes))
      (call $zstd_reset)
      (local.set $in_buf (call $zstd_alloc (local.get $len)))
      (local.set $out_buf (call $zstd_alloc (local.get $out_len)))
      (call $bytes_to_mem
         (local.get $input) (local.get $pos) (local.get $in_buf) (local.get $len))
      (if (i32.ne
             (call $zstd_decompress
                (local.get $out_buf) (local.get $out_len)
                (local.get $in_buf) (local.get $len))
             (local.get $out_len))
         (then (call $caml_failwith (global.get $decompress_failed))))
      (local.set $output (array.new $bytes (i32.const 0) (local.get $out_len)))
      (call $mem_to_bytes
         (local.get $out_buf) (local.get $output) (i32.const 0) (local.get $out_len))
      (local.get $output))

   ;; Stream a large frame through zstd's fixed chunk buffers, copying the
   ;; compressed input in and the plaintext out a chunk at a time, so neither
   ;; the input nor the full output is ever materialised in linear memory.
   (func $decompress_stream
      (param $input (ref $bytes)) (param $pos i32) (param $len i32)
      (param $out_len i32) (result (ref $bytes))
      (local $output (ref $bytes))
      (local $src_ofs i32) (local $src_end i32) (local $out_ofs i32)
      (local $avail i32) (local $ret i32) (local $produced i32)
      (local $need_input i32)
      (if (i32.lt_s (call $stream_init) (i32.const 0))
         (then (call $caml_failwith (global.get $decompress_failed))))
      (local.set $output (array.new $bytes (i32.const 0) (local.get $out_len)))
      (local.set $src_ofs (local.get $pos))
      (local.set $src_end (i32.add (local.get $pos) (local.get $len)))
      (local.set $need_input (i32.const 1))
      (block $done
         (loop $loop
            ;; Refill the input chunk once zstd has drained the previous one.
            (if (i32.or (local.get $need_input) (call $in_done))
               (then
                  (local.set $avail
                     (i32.sub (local.get $src_end) (local.get $src_ofs)))
                  (if (i32.gt_u (local.get $avail) (call $in_cap))
                     (then (local.set $avail (call $in_cap))))
                  ;; Input exhausted before the frame ended: truncated data.
                  (br_if $done (i32.eqz (local.get $avail)))
                  (call $bytes_to_mem
                     (local.get $input) (local.get $src_ofs)
                     (call $in_buf) (local.get $avail))
                  (local.set $src_ofs
                     (i32.add (local.get $src_ofs) (local.get $avail)))
                  (call $stream_set_input (local.get $avail))
                  (local.set $need_input (i32.const 0))))
            (local.set $ret (call $stream_run))
            (if (i32.lt_s (local.get $ret) (i32.const 0))
               (then (call $caml_failwith (global.get $decompress_failed))))
            (local.set $produced (call $out_produced))
            ;; Guard against a frame that decodes to more than out_len.
            (if (i32.gt_u (i32.add (local.get $out_ofs) (local.get $produced))
                   (local.get $out_len))
               (then (call $caml_failwith (global.get $decompress_failed))))
            (call $mem_to_bytes
               (call $out_buf) (local.get $output) (local.get $out_ofs)
               (local.get $produced))
            (local.set $out_ofs (i32.add (local.get $out_ofs) (local.get $produced)))
            (br_if $done (i32.eqz (local.get $ret)))  ;; frame complete
            (if (call $in_done)
               (then (local.set $need_input (i32.const 1))))
            (br $loop)))
      ;; A well-formed frame decodes to exactly out_len bytes.
      (if (i32.ne (local.get $out_ofs) (local.get $out_len))
         (then (call $caml_failwith (global.get $decompress_failed))))
      (local.get $output))

   ;; Peek at the frame's declared window size, then decode whichever way
   ;; keeps the footprint smallest: one-shot when the output fits in the
   ;; window (the output buffer then doubles as the window, so nothing larger
   ;; than the window is allocated either way), streaming when it is larger
   ;; (zstd keeps just a window-sized history buffer instead of the whole
   ;; output).  An unparsable header yields window 0, routing to streaming,
   ;; which then fails cleanly on the bad frame.
   (func $decompress
      (param $input (ref $bytes)) (param $pos i32) (param $len i32)
      (param $out_len i32) (result (ref $bytes))
      (local $hdr i32)
      (local.set $hdr (local.get $len))
      (if (i32.gt_u (local.get $hdr) (global.get $frame_header_max))
         (then (local.set $hdr (global.get $frame_header_max))))
      ;; The header peek reuses the streaming input chunk as scratch; both
      ;; decode paths overwrite or ignore it afterwards.
      (call $bytes_to_mem
         (local.get $input) (local.get $pos) (call $in_buf) (local.get $hdr))
      (if (result (ref $bytes))
         (i32.le_u (local.get $out_len)
            (call $window_size (call $in_buf) (local.get $hdr)))
         (then
            (call $decompress_oneshot
               (local.get $input) (local.get $pos) (local.get $len)
               (local.get $out_len)))
         (else
            (call $decompress_stream
               (local.get $input) (local.get $pos) (local.get $len)
               (local.get $out_len)))))

   (func (export "caml_zstd_initialize") (param (ref eq)) (result (ref eq))
      (global.set $caml_intern_decompress_input (ref.func $decompress))
      (ref.i31 (i32.const 1)))
)
(@else
   (func (export "caml_zstd_initialize") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
))
)
