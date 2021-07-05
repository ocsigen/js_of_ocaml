(** A [result] type for all the toplevel functions. *)
type 'a result =
  | Success of 'a * warning list
  | Error of error * warning list

and error =
  { msg : string
  ; locs : loc list }

and warning = error

and loc =
  { loc_start : int * int
  ; loc_end : int * int }

include
  JsooTopIntf.Wrapped
    with type toplevel := unit
     and type 'a result := 'a result
     and type output := Format.formatter

val error_of_exn : exn -> error
