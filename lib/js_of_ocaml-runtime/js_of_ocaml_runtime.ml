(* This local module [Js] is needed so that the ppx_js extension work within that file. *)
module Js = struct
  type +'a t

  type (-'a, +'b) meth_callback

  module Unsafe = struct
    type top

    type any = top t

    type any_js_array = any

    external inject : 'a -> any = "%identity"

    external coerce : _ t -> _ t = "%identity"

    external get : 'a -> 'b -> 'c = "caml_js_get"

    external set : 'a -> 'b -> 'c -> unit = "caml_js_set"

    external delete : 'a -> 'b -> unit = "caml_js_delete"

    external call : 'a -> 'b -> any array -> 'c = "caml_js_call"

    external fun_call : 'a -> any array -> 'b = "caml_js_fun_call"

    external meth_call : 'a -> string -> any array -> 'b = "caml_js_meth_call"

    external new_obj : 'a -> any array -> 'b = "caml_js_new"

    external new_obj_arr : 'a -> any_js_array -> 'b = "caml_ojs_new_arr"

    external obj : (string * any) array -> 'a = "caml_js_object"

    external equals : 'a -> 'b -> bool = "caml_js_equals"

    external pure_expr : (unit -> 'a) -> 'a = "caml_js_pure_expr"

    external eval_string : string -> 'a = "caml_js_eval_string"

    external js_expr : string -> 'a = "caml_js_expr"

    external pure_js_expr : string -> 'a = "caml_pure_js_expr"

    external callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback = "%identity"

    external callback_with_arguments :
      (any_js_array -> 'b) -> ('c, any_js_array -> 'b) meth_callback
      = "caml_js_wrap_callback_arguments"

    external callback_with_arity : int -> ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
      = "caml_js_wrap_callback_strict"

    external meth_callback : ('b -> 'a) -> ('b, 'a) meth_callback
      = "caml_js_wrap_meth_callback_unsafe"

    external meth_callback_with_arity : int -> ('b -> 'a) -> ('b, 'a) meth_callback
      = "caml_js_wrap_meth_callback_strict"

    external meth_callback_with_arguments :
      ('b -> any_js_array -> 'a) -> ('b, any_js_array -> 'a) meth_callback
      = "caml_js_wrap_meth_callback_arguments"

    (* DEPRECATED *)
    external variable : string -> 'a = "caml_js_var"
  end

  external debugger : unit -> unit = "debugger"

  (****)

  type 'a opt = 'a

  type 'a optdef = 'a

  (****)

  type +'a meth

  type +'a gen_prop

  type 'a readonly_prop = < get : 'a > gen_prop

  type 'a writeonly_prop = < set : 'a -> unit > gen_prop

  type 'a prop = < get : 'a ; set : 'a -> unit > gen_prop

  type 'a optdef_prop = < get : 'a optdef ; set : 'a -> unit > gen_prop

  type +'a constr

  (****)

  type 'a callback = (unit, 'a) meth_callback

  external wrap_callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
    = "caml_js_wrap_callback"

  external wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback
    = "caml_js_wrap_meth_callback"

  (****)

  type match_result_handle

  type string_array

  class type js_string =
    object
      method toString : js_string t meth

      method valueOf : js_string t meth

      method charAt : int -> js_string t meth

      method charCodeAt : int -> float meth

      (* This may return NaN... *)
      method concat : js_string t -> js_string t meth

      method concat_2 : js_string t -> js_string t -> js_string t meth

      method concat_3 : js_string t -> js_string t -> js_string t -> js_string t meth

      method concat_4 :
        js_string t -> js_string t -> js_string t -> js_string t -> js_string t meth

      method indexOf : js_string t -> int meth

      method indexOf_from : js_string t -> int -> int meth

      method lastIndexOf : js_string t -> int meth

      method lastIndexOf_from : js_string t -> int -> int meth

      method localeCompare : js_string t -> float meth

      method _match : regExp t -> match_result_handle t opt meth

      method replace : regExp t -> js_string t -> js_string t meth

      method replace_string : js_string t -> js_string t -> js_string t meth

      method search : regExp t -> int meth

      method slice : int -> int -> js_string t meth

      method slice_end : int -> js_string t meth

      method split : js_string t -> string_array t meth

      method split_limited : js_string t -> int -> string_array t meth

      method split_regExp : regExp t -> string_array t meth

      method split_regExpLimited : regExp t -> int -> string_array t meth

      method substring : int -> int -> js_string t meth

      method substring_toEnd : int -> js_string t meth

      method toLowerCase : js_string t meth

      method toLocaleLowerCase : js_string t meth

      method toUpperCase : js_string t meth

      method toLocaleUpperCase : js_string t meth

      method trim : js_string t meth

      method length : int readonly_prop
    end

  and regExp =
    object
      method exec : js_string t -> match_result_handle t opt meth

      method test : js_string t -> bool t meth

      method toString : js_string t meth

      method source : js_string t readonly_prop

      method global : bool t readonly_prop

      method ignoreCase : bool t readonly_prop

      method multiline : bool t readonly_prop

      method lastIndex : int prop
    end

  (* string is used by ppx_js, it needs to come before any use of the
     new syntax in this file *)
  external string : string -> js_string t = "caml_jsstring_of_string"

  external to_string : js_string t -> string = "caml_string_of_jsstring"

  class type error =
    object
      method name : js_string t prop

      method message : js_string t prop

      method stack : js_string t optdef prop

      method toString : js_string t meth
    end

  external js_error_of_exn : exn -> error t opt = "caml_js_error_of_exception"

  external exn_with_js_backtrace : exn -> force:bool -> exn = "caml_exn_with_js_backtrace"
end

module Js_error = struct
  type t = Js.error Js.t

  exception Exn of t

  let () = Callback.register_exception "jsError" (Exn (Obj.magic [||]))
end
