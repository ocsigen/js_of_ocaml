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

   (type $block (array (mut (ref eq))))

   (global $caml_custom_event_index (mut i32) (i32.const 0))

   (func (export "caml_runtime_events_user_register")
      (param $evname (ref eq)) (param $evtag (ref eq)) (param $evtype (ref eq))
      (result (ref eq))
      (global.set $caml_custom_event_index
         (i32.add (global.get $caml_custom_event_index) (i32.const 1)))
      (array.new_fixed $block 5
         (ref.i31 (i32.const 0))
         (ref.i31 (global.get $caml_custom_event_index))
         (local.get $evname)
         (local.get $evtag)
         (local.get $evtype)))

(@if (>= ocaml_version (5 2 0))
(@then
   (func (export "caml_runtime_events_user_write")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
(@else
   (func (export "caml_runtime_events_user_write")
      (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
))

   (func (export "caml_runtime_events_user_resolve")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_runtime_events_start")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_runtime_events_pause")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_runtime_events_resume")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_runtime_events_are_active")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_runtime_events_create_cursor")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_runtime_events_free_cursor")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_runtime_events_read_poll")
      (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_ml_runtime_events_path")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))
)
