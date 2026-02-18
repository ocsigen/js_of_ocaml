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
   ;; Imports from other wasm modules
   (import "fail" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "fail" "caml_raise_with_arg"
      (func $caml_raise_with_arg (param (ref eq)) (param (ref eq))))
   (import "stdlib" "caml_named_value"
      (func $caml_named_value (param (ref eq)) (result (ref null eq))))
   (import "jslib" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "jslib" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "jslib" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "bindings" "identity"
      (func $from_int32 (param i32) (result anyref)))
   (import "bindings" "new_array"
      (func $new_array (param i32) (result (ref extern))))
   (import "bindings" "array_set"
      (func $array_set (param (ref extern)) (param i32) (param anyref)))

   ;; JS helper imports
   (import "js" "gr_state_for_wasm"
      (func $gr_state (result anyref)))
   (import "js" "gr_open_for_wasm"
      (func $gr_open (param anyref) (result i32)))
   (import "js" "gr_close_for_wasm" (func $gr_close))
   (import "js" "gr_clear_for_wasm" (func $gr_clear))
   (import "js" "gr_size_x_for_wasm" (func $gr_size_x (result i32)))
   (import "js" "gr_size_y_for_wasm" (func $gr_size_y (result i32)))
   (import "js" "gr_current_x_for_wasm" (func $gr_current_x (result i32)))
   (import "js" "gr_current_y_for_wasm" (func $gr_current_y (result i32)))
   (import "js" "gr_set_color_for_wasm" (func $gr_set_color (param i32)))
   (import "js" "gr_plot_for_wasm" (func $gr_plot (param i32 i32)))
   (import "js" "gr_point_color_for_wasm"
      (func $gr_point_color (param i32 i32) (result i32)))
   (import "js" "gr_moveto_for_wasm" (func $gr_moveto (param i32 i32)))
   (import "js" "gr_lineto_for_wasm" (func $gr_lineto (param i32 i32)))
   (import "js" "gr_draw_rect_for_wasm"
      (func $gr_draw_rect (param i32 i32 i32 i32)))
   (import "js" "gr_fill_rect_for_wasm"
      (func $gr_fill_rect (param i32 i32 i32 i32)))
   (import "js" "gr_draw_arc_for_wasm"
      (func $gr_draw_arc (param i32 i32 i32 i32 i32 i32)))
   (import "js" "gr_fill_arc_for_wasm"
      (func $gr_fill_arc (param i32 i32 i32 i32 i32 i32)))
   (import "js" "gr_set_line_width_for_wasm"
      (func $gr_set_line_width (param i32)))
   (import "js" "gr_resize_window_for_wasm"
      (func $gr_resize_window (param i32 i32)))
   (import "js" "gr_draw_char_for_wasm" (func $gr_draw_char (param i32)))
   (import "js" "gr_draw_str_for_wasm" (func $gr_draw_str (param anyref)))
   (import "js" "gr_set_font_for_wasm" (func $gr_set_font (param anyref)))
   (import "js" "gr_set_text_size_for_wasm"
      (func $gr_set_text_size (param i32)))
   (import "js" "gr_set_window_title_for_wasm"
      (func $gr_set_window_title (param anyref)))
   (import "js" "gr_text_size_w_for_wasm"
      (func $gr_text_size_w (param anyref) (result i32)))
   (import "js" "gr_text_size_h_for_wasm"
      (func $gr_text_size_h (result i32)))
   (import "js" "gr_fill_poly_for_wasm"
      (func $gr_fill_poly (param anyref i32)))
   (import "js" "gr_create_image_for_wasm"
      (func $gr_create_image (param i32 i32) (result anyref)))
   (import "js" "gr_draw_image_for_wasm"
      (func $gr_draw_image (param anyref i32 i32)))
   (import "js" "gr_blit_image_for_wasm"
      (func $gr_blit_image (param anyref i32 i32)))
   (import "js" "gr_make_image_for_wasm"
      (func $gr_make_image (param anyref i32 i32) (result anyref)))
   (import "js" "gr_dump_image_width_for_wasm"
      (func $gr_dump_image_width (param anyref) (result i32)))
   (import "js" "gr_dump_image_height_for_wasm"
      (func $gr_dump_image_height (param anyref) (result i32)))
   (import "js" "gr_dump_image_pixel_for_wasm"
      (func $gr_dump_image_pixel (param anyref i32 i32) (result i32)))
   (import "js" "gr_state_set_for_wasm"
      (func $gr_state_set (param anyref)))
   (import "js" "gr_state_create_for_wasm"
      (func $gr_state_create (param anyref i32 i32) (result anyref)))
   (import "js" "gr_doc_of_state_for_wasm"
      (func $gr_doc_of_state (param anyref) (result anyref)))

   ;; Types
   (type $block (array (mut (ref eq))))
   (type $bytes (array (mut i8)))
   (type $js (struct (field anyref)))

   ;; String constants
   (@string $graphic_failure "Graphics.Graphic_failure")
   (@string $not_initialized "Not initialized")
   (@string $open_failed "Graphics.open_graph: cannot open the window")
   (@string $wait_event
      "caml_gr_wait_event not Implemented: use Graphics_js instead")
   (@string $synchronize "caml_gr_synchronize not Implemented")
   (@string $remember_mode "caml_gr_remember_mode not Implemented")
   (@string $display_mode "caml_gr_display_mode not Implemented")
   (@string $window_id "caml_gr_window_id not Implemented")
   (@string $open_subwindow "caml_gr_open_subwindow not Implemented")
   (@string $close_subwindow "caml_gr_close_subwindow not Implemented")

   ;; State check helper: raises Graphics.Graphic_failure if not initialized
   (func $check_state
      (if (ref.is_null (call $gr_state))
         (then
            (block $no_named_value
               (call $caml_raise_with_arg
                  (br_on_null $no_named_value
                     (call $caml_named_value (global.get $graphic_failure)))
                  (global.get $not_initialized))
               (return))
            (call $caml_failwith (global.get $not_initialized)))))

   ;; --- Open / Close / Clear ---

   (func (export "caml_gr_open_graph")
      (param (ref eq)) (result (ref eq))
      (if (call $gr_open
             (call $unwrap (call $caml_jsstring_of_string (local.get 0))))
         (then (call $caml_failwith (global.get $open_failed))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_close_graph")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_close)
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_clear_graph")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_clear)
      (ref.i31 (i32.const 0)))

   ;; --- Size / Position getters ---

   (func (export "caml_gr_size_x")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (ref.i31 (call $gr_size_x)))

   (func (export "caml_gr_size_y")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (ref.i31 (call $gr_size_y)))

   (func (export "caml_gr_current_x")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (ref.i31 (call $gr_current_x)))

   (func (export "caml_gr_current_y")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (ref.i31 (call $gr_current_y)))

   ;; --- Drawing with ints ---

   (func (export "caml_gr_set_color")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_set_color
         (i31.get_s (ref.cast (ref i31) (local.get 0))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_plot")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_plot
         (i31.get_s (ref.cast (ref i31) (local.get 0)))
         (i31.get_s (ref.cast (ref i31) (local.get 1))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_point_color")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $check_state)
      (ref.i31
         (call $gr_point_color
            (i31.get_s (ref.cast (ref i31) (local.get 0)))
            (i31.get_s (ref.cast (ref i31) (local.get 1))))))

   (func (export "caml_gr_moveto")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_moveto
         (i31.get_s (ref.cast (ref i31) (local.get 0)))
         (i31.get_s (ref.cast (ref i31) (local.get 1))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_lineto")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_lineto
         (i31.get_s (ref.cast (ref i31) (local.get 0)))
         (i31.get_s (ref.cast (ref i31) (local.get 1))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_draw_rect")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      (call $check_state)
      (call $gr_draw_rect
         (i31.get_s (ref.cast (ref i31) (local.get 0)))
         (i31.get_s (ref.cast (ref i31) (local.get 1)))
         (i31.get_s (ref.cast (ref i31) (local.get 2)))
         (i31.get_s (ref.cast (ref i31) (local.get 3))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_fill_rect")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      (call $check_state)
      (call $gr_fill_rect
         (i31.get_s (ref.cast (ref i31) (local.get 0)))
         (i31.get_s (ref.cast (ref i31) (local.get 1)))
         (i31.get_s (ref.cast (ref i31) (local.get 2)))
         (i31.get_s (ref.cast (ref i31) (local.get 3))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_draw_arc")
      (param (ref eq)) (param (ref eq)) (param (ref eq))
      (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      (call $check_state)
      (call $gr_draw_arc
         (i31.get_s (ref.cast (ref i31) (local.get 0)))
         (i31.get_s (ref.cast (ref i31) (local.get 1)))
         (i31.get_s (ref.cast (ref i31) (local.get 2)))
         (i31.get_s (ref.cast (ref i31) (local.get 3)))
         (i31.get_s (ref.cast (ref i31) (local.get 4)))
         (i31.get_s (ref.cast (ref i31) (local.get 5))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_fill_arc")
      (param (ref eq)) (param (ref eq)) (param (ref eq))
      (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      (call $check_state)
      (call $gr_fill_arc
         (i31.get_s (ref.cast (ref i31) (local.get 0)))
         (i31.get_s (ref.cast (ref i31) (local.get 1)))
         (i31.get_s (ref.cast (ref i31) (local.get 2)))
         (i31.get_s (ref.cast (ref i31) (local.get 3)))
         (i31.get_s (ref.cast (ref i31) (local.get 4)))
         (i31.get_s (ref.cast (ref i31) (local.get 5))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_set_line_width")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_set_line_width
         (i31.get_s (ref.cast (ref i31) (local.get 0))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_resize_window")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_resize_window
         (i31.get_s (ref.cast (ref i31) (local.get 0)))
         (i31.get_s (ref.cast (ref i31) (local.get 1))))
      (ref.i31 (i32.const 0)))

   ;; --- Text / String operations ---

   (func (export "caml_gr_draw_char")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_draw_char
         (i31.get_s (ref.cast (ref i31) (local.get 0))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_draw_string")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_draw_str
         (call $unwrap (call $caml_jsstring_of_string (local.get 0))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_set_font")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_set_font
         (call $unwrap (call $caml_jsstring_of_string (local.get 0))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_set_text_size")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_set_text_size
         (i31.get_s (ref.cast (ref i31) (local.get 0))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_set_window_title")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_set_window_title
         (call $unwrap (call $caml_jsstring_of_string (local.get 0))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_text_size")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (array.new_fixed $block 3
         (ref.i31 (i32.const 0))
         (ref.i31
            (call $gr_text_size_w
               (call $unwrap (call $caml_jsstring_of_string (local.get 0)))))
         (ref.i31 (call $gr_text_size_h))))

   ;; --- Polygon ---

   (func (export "caml_gr_fill_poly")
      (param $ar (ref eq)) (result (ref eq))
      (local $a (ref $block))
      (local $p (ref $block))
      (local $flat (ref extern))
      (local $i i32) (local $n i32)
      (call $check_state)
      (local.set $a (ref.cast (ref $block) (local.get $ar)))
      (local.set $n (i32.sub (array.len (local.get $a)) (i32.const 1)))
      (local.set $flat
         (call $new_array (i32.mul (local.get $n) (i32.const 2))))
      (local.set $i (i32.const 0))
      (loop $loop
         (if (i32.lt_u (local.get $i) (local.get $n))
            (then
               (local.set $p
                  (ref.cast (ref $block)
                     (array.get $block (local.get $a)
                        (i32.add (local.get $i) (i32.const 1)))))
               (call $array_set (local.get $flat)
                  (i32.mul (local.get $i) (i32.const 2))
                  (call $from_int32
                     (i31.get_s (ref.cast (ref i31)
                        (array.get $block (local.get $p) (i32.const 1))))))
               (call $array_set (local.get $flat)
                  (i32.add (i32.mul (local.get $i) (i32.const 2))
                     (i32.const 1))
                  (call $from_int32
                     (i31.get_s (ref.cast (ref i31)
                        (array.get $block (local.get $p) (i32.const 2))))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (call $gr_fill_poly
         (any.convert_extern (local.get $flat)) (local.get $n))
      (ref.i31 (i32.const 0)))

   ;; --- Image operations ---

   (func (export "caml_gr_create_image")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $check_state)
      (struct.new $js
         (call $gr_create_image
            (i31.get_s (ref.cast (ref i31) (local.get 0)))
            (i31.get_s (ref.cast (ref i31) (local.get 1))))))

   (func (export "caml_gr_draw_image")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_draw_image
         (call $unwrap (local.get 0))
         (i31.get_s (ref.cast (ref i31) (local.get 1)))
         (i31.get_s (ref.cast (ref i31) (local.get 2))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_blit_image")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $gr_blit_image
         (call $unwrap (local.get 0))
         (i31.get_s (ref.cast (ref i31) (local.get 1)))
         (i31.get_s (ref.cast (ref i31) (local.get 2))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_make_image")
      (param $arr (ref eq)) (result (ref eq))
      (local $a (ref $block))
      (local $row (ref $block))
      (local $flat (ref extern))
      (local $h i32) (local $w i32) (local $i i32) (local $j i32)
      (local $idx i32)
      (call $check_state)
      (local.set $a (ref.cast (ref $block) (local.get $arr)))
      (local.set $h (i32.sub (array.len (local.get $a)) (i32.const 1)))
      (local.set $row
         (ref.cast (ref $block)
            (array.get $block (local.get $a) (i32.const 1))))
      (local.set $w (i32.sub (array.len (local.get $row)) (i32.const 1)))
      (local.set $flat
         (call $new_array (i32.mul (local.get $h) (local.get $w))))
      (local.set $idx (i32.const 0))
      (local.set $i (i32.const 0))
      (loop $outer
         (if (i32.lt_u (local.get $i) (local.get $h))
            (then
               (local.set $row
                  (ref.cast (ref $block)
                     (array.get $block (local.get $a)
                        (i32.add (local.get $i) (i32.const 1)))))
               (local.set $j (i32.const 0))
               (loop $inner
                  (if (i32.lt_u (local.get $j) (local.get $w))
                     (then
                        (call $array_set (local.get $flat) (local.get $idx)
                           (call $from_int32
                              (i31.get_s (ref.cast (ref i31)
                                 (array.get $block (local.get $row)
                                    (i32.add (local.get $j)
                                       (i32.const 1)))))))
                        (local.set $idx
                           (i32.add (local.get $idx) (i32.const 1)))
                        (local.set $j
                           (i32.add (local.get $j) (i32.const 1)))
                        (br $inner))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $outer))))
      (struct.new $js
         (call $gr_make_image
            (any.convert_extern (local.get $flat))
            (local.get $w) (local.get $h))))

   (func (export "caml_gr_dump_image")
      (param $im (ref eq)) (result (ref eq))
      (local $js_im anyref)
      (local $w i32) (local $h i32)
      (local $result (ref $block))
      (local $row (ref $block))
      (local $i i32) (local $j i32)
      (local.set $js_im (call $unwrap (local.get $im)))
      (local.set $w (call $gr_dump_image_width (local.get $js_im)))
      (local.set $h (call $gr_dump_image_height (local.get $js_im)))
      (local.set $result
         (array.new $block (ref.i31 (i32.const 0))
            (i32.add (local.get $h) (i32.const 1))))
      (local.set $i (i32.const 0))
      (loop $outer
         (if (i32.lt_u (local.get $i) (local.get $h))
            (then
               (local.set $row
                  (array.new $block (ref.i31 (i32.const 0))
                     (i32.add (local.get $w) (i32.const 1))))
               (local.set $j (i32.const 0))
               (loop $inner
                  (if (i32.lt_u (local.get $j) (local.get $w))
                     (then
                        (array.set $block (local.get $row)
                           (i32.add (local.get $j) (i32.const 1))
                           (ref.i31
                              (call $gr_dump_image_pixel
                                 (local.get $js_im)
                                 (local.get $i) (local.get $j))))
                        (local.set $j
                           (i32.add (local.get $j) (i32.const 1)))
                        (br $inner))))
               (array.set $block (local.get $result)
                  (i32.add (local.get $i) (i32.const 1))
                  (local.get $row))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $outer))))
      (local.get $result))

   ;; --- Graphics_js state management ---

   (func (export "caml_gr_state_get")
      (param (ref eq)) (result (ref eq))
      (call $check_state)
      (call $wrap (call $gr_state)))

   (func (export "caml_gr_state_set")
      (param (ref eq)) (result (ref eq))
      (call $gr_state_set (call $unwrap (local.get 0)))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_state_create")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (call $wrap
         (call $gr_state_create
            (call $unwrap (local.get 0))
            (i31.get_s (ref.cast (ref i31) (local.get 1)))
            (i31.get_s (ref.cast (ref i31) (local.get 2))))))

   (func (export "caml_gr_doc_of_state")
      (param (ref eq)) (result (ref eq))
      (call $wrap (call $gr_doc_of_state (call $unwrap (local.get 0)))))

   ;; --- Stubs returning unit ---

   (func (export "caml_gr_sigio_handler")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_sigio_signal")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   ;; --- Stubs raising Failure ---

   (func (export "caml_gr_wait_event")
      (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $wait_event))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_synchronize")
      (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $synchronize))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_remember_mode")
      (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $remember_mode))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_display_mode")
      (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $display_mode))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_window_id")
      (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $window_id))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_open_subwindow")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
      (result (ref eq))
      (call $caml_failwith (global.get $open_subwindow))
      (ref.i31 (i32.const 0)))

   (func (export "caml_gr_close_subwindow")
      (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $close_subwindow))
      (ref.i31 (i32.const 0)))
)
