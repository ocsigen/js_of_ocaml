
open Js

class type console = object
  method log : _ -> unit meth
  method log_2 : _ -> _ -> unit meth
  method log_3 : _ -> _ -> _ -> unit meth
  method log_4 : _ -> _ -> _ -> _ -> unit meth
  method log_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method debug : _ -> unit meth
  method debug_2 : _ -> _ -> unit meth
  method debug_3 : _ -> _ -> _ -> unit meth
  method debug_4 : _ -> _ -> _ -> _ -> unit meth
  method debug_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method info : _ -> unit meth
  method info_2 : _ -> _ -> unit meth
  method info_3 : _ -> _ -> _ -> unit meth
  method info_4 : _ -> _ -> _ -> _ -> unit meth
  method info_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method warn : _ -> unit meth
  method warn_2 : _ -> _ -> unit meth
  method warn_3 : _ -> _ -> _ -> unit meth
  method warn_4 : _ -> _ -> _ -> _ -> unit meth
  method warn_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method error : _ -> unit meth
  method error_2 : _ -> _ -> unit meth
  method error_3 : _ -> _ -> _ -> unit meth
  method error_4 : _ -> _ -> _ -> _ -> unit meth
  method error_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method assert_ : bool t -> unit meth
  method assert_1 : bool t -> _ -> unit meth
  method assert_2 : bool t -> _ -> _ -> unit meth
  method assert_3 : bool t -> _ -> _ -> _ -> unit meth
  method assert_4 : bool t -> _ -> _ -> _ -> _ -> unit meth
  method assert_5 : bool t -> _ -> _ -> _ -> _ -> _ -> unit meth
  method dir : _ -> unit meth
  method dirxml : #Dom.node t -> unit meth
  method trace : unit meth
  method group : _ -> unit meth
  method group_2 : _ -> _ -> unit meth
  method group_3 : _ -> _ -> _ -> unit meth
  method group_4 : _ -> _ -> _ -> _ -> unit meth
  method group_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method groupCollapsed : _ -> unit meth
  method groupCollapsed_2 : _ -> _ -> unit meth
  method groupCollapsed_3 : _ -> _ -> _ -> unit meth
  method groupCollapsed_4 : _ -> _ -> _ -> _ -> unit meth
  method groupCollapsed_5 : _ -> _ -> _ -> _ -> _ -> unit meth
  method groupEnd : unit meth
  method time : js_string t -> unit meth
  method timeEnd : js_string t -> unit meth
end

let console : console t =
  Js.Unsafe.fun_call (Js.Unsafe.variable "caml_get_console") [||]
