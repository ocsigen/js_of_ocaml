module Js_error = struct
  type t

  exception Exn of t

  let () = Callback.register_exception "jsError" (Exn (Obj.magic [||]))
end
