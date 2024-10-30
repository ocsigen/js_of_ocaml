module Js = struct
  type t

  type 'a js_array = t

  type ('a, 'b) meth_callback = t

  external string : string -> t = "caml_jsstring_of_string"

  external to_string : t -> string = "caml_string_of_jsstring"

  external bytestring : string -> t = "caml_jsbytes_of_string"

  external to_bytestring : t -> string = "caml_string_of_jsbytes"

  external bool : bool -> t = "caml_js_from_bool"

  external to_bool : t -> bool = "caml_js_to_bool"

  external array : 'a array -> t = "caml_js_from_array"

  external to_array : t -> 'a array = "caml_js_to_array"

  external number_of_float : float -> t = "caml_js_from_float"

  external float_of_number : t -> float = "caml_js_to_float"

  external number_of_int32 : int32 -> t = "caml_js_from_int32"

  external int32_of_number : t -> int32 = "caml_js_to_int32"

  external number_of_nativeint : nativeint -> t = "caml_js_from_nativeint"

  external nativeint_of_number : t -> nativeint = "caml_js_to_nativeint"

  external typeof : t -> t = "caml_js_typeof"

  external instanceof : t -> t -> bool = "caml_js_instanceof"

  external debugger : unit -> unit = "debugger"

  external get : t -> t -> t = "caml_js_get"

  external set : t -> t -> t -> unit = "caml_js_set"

  external delete : t -> t -> unit = "caml_js_delete"

  external call : t -> t -> t array -> t = "caml_js_call"

  external fun_call : t -> t array -> t = "caml_js_fun_call"

  external meth_call : t -> string -> t array -> t = "caml_js_meth_call"

  external new_obj : t -> t array -> t = "caml_js_new"

  external new_obj_arr : t -> t js_array -> t = "caml_ojs_new_arr"

  external obj : (string * t) array -> t = "caml_js_object"

  external equals : t -> t -> bool = "caml_js_equals"

  external strict_equals : t -> t -> bool = "caml_js_strict_equals"

  external pure_expr : (unit -> 'a) -> 'a = "caml_js_pure_expr"

  external eval_string : string -> 'a = "caml_js_eval_string"

  external js_expr : string -> 'a = "caml_js_expr"

  external pure_js_expr : string -> 'a = "caml_pure_js_expr"

  external callback : ('b -> 'a) -> ('b, 'a) meth_callback
    = "caml_js_wrap_callback_unsafe"

  external callback_with_arguments :
    (t js_array -> 'b) -> ('c, t js_array -> 'b) meth_callback
    = "caml_js_wrap_callback_arguments"

  external callback_with_arity : int -> ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
    = "caml_js_wrap_callback_strict"

  external meth_callback : ('b -> 'a) -> ('b, 'a) meth_callback
    = "caml_js_wrap_meth_callback_unsafe"

  external meth_callback_with_arity : int -> ('b -> 'a) -> ('b, 'a) meth_callback
    = "caml_js_wrap_meth_callback_strict"

  external meth_callback_with_arguments :
    ('b -> t js_array -> 'a) -> ('b, t js_array -> 'a) meth_callback
    = "caml_js_wrap_meth_callback_arguments"

  external wrap_callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
    = "caml_js_wrap_callback"

  external wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback
    = "caml_js_wrap_meth_callback"
end

module Sys = struct
  type 'a callback = 'a

  external create_file : name:string -> content:string -> unit = "caml_create_file"

  external read_file : name:string -> string = "caml_read_file_content"

  external set_channel_output' : out_channel -> (js_string:Js.t -> unit) callback -> unit
    = "caml_ml_set_channel_output"

  external set_channel_input' : in_channel -> (unit -> string) callback -> unit
    = "caml_ml_set_channel_refill"

  external mount_point : unit -> string list = "caml_list_mount_point"

  external mount_autoload : string -> (string -> string -> string option) callback -> unit
    = "caml_mount_autoload"

  external unmount : string -> unit = "caml_unmount"

  type redirection

  external redirect_channel : out_channel -> into:out_channel -> redirection
    = "caml_ml_channel_redirect"

  external restore_channel : out_channel -> redirection -> unit
    = "caml_ml_channel_restore"

  module Config = struct
    external use_js_string : unit -> bool = "caml_jsoo_flags_use_js_string"

    external effects : unit -> bool = "caml_jsoo_flags_effects"
  end

  let version = Runtime_version.s

  let git_version = Runtime_version.git_version
end

module Error : sig
  type t

  val raise_ : t -> 'a

  val attach_js_backtrace : exn -> force:bool -> exn
  (** Attach a JavasScript error to an OCaml exception. if [force = false] and a
      JavasScript error is already attached, it will do nothing. This function is useful
      to store and retrieve information about JavaScript stack traces.

      Attaching JavasScript errors will happen automatically when compiling with
      [--enable with-js-error]. *)

  val of_exn : exn -> t option
  (** Extract a JavaScript error attached to an OCaml exception, if any. This is useful to
      inspect an eventual stack strace, especially when sourcemap is enabled. *)

  exception Exn of t
  (** The [Error] exception wrap javascript exceptions when caught by OCaml code. In case
      the javascript exception is not an instance of javascript [Error], it will be
      serialized and wrapped into a [Failure] exception. *)
end = struct
  type t

  exception Exn of t

  let _ = Callback.register_exception "jsError" (Exn (Obj.magic [||]))

  let raise_ : t -> 'a = Js.js_expr "(function (exn) { throw exn })"

  external of_exn : exn -> t option = "caml_js_error_option_of_exception"

  external attach_js_backtrace : exn -> force:bool -> exn = "caml_exn_with_js_backtrace"
end

[@@@ocaml.warning "-32-60"]

module For_compatibility_only = struct
  (* Add primitives for compatibility reasons. Existing users might
     depend on it (e.g. gen_js_api), we dont want the ocaml compiler
     to complain about theses missing primitives. *)

  external caml_js_from_string : string -> Js.t = "caml_js_from_string"

  external caml_js_to_byte_string : Js.t -> string = "caml_js_to_byte_string"

  external caml_js_to_string : Js.t -> string = "caml_js_to_string"

  external caml_list_of_js_array : 'a Js.js_array -> 'a list = "caml_list_of_js_array"

  external caml_list_to_js_array : 'a list -> 'a Js.js_array = "caml_list_to_js_array"

  external variable : string -> 'a = "caml_js_var"
end

module Typed_array = struct
  type ('a, 'b) typedArray = Js.t

  type arrayBuffer = Js.t

  type uint8Array = Js.t

  external kind : ('a, 'b) typedArray -> ('a, 'b) Bigarray.kind
    = "caml_ba_kind_of_typed_array"

  external from_genarray :
    ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> ('a, 'b) typedArray
    = "caml_ba_to_typed_array"

  external to_genarray :
    ('a, 'b) typedArray -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t
    = "caml_ba_from_typed_array"

  module Bigstring = struct
    type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

    external to_arrayBuffer : t -> arrayBuffer = "bigstring_to_array_buffer"

    external to_uint8Array : t -> uint8Array = "bigstring_to_typed_array"

    external of_arrayBuffer : arrayBuffer -> t = "bigstring_of_array_buffer"

    external of_uint8Array : uint8Array -> t = "bigstring_of_typed_array"
  end

  external of_uint8Array : uint8Array -> string = "caml_string_of_array"
end

module Int64 = struct
  external create_int64_lo_mi_hi : int -> int -> int -> Int64.t
    = "caml_int64_create_lo_mi_hi"
end
