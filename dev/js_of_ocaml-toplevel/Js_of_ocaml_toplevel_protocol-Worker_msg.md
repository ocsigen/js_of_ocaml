
# Module `Js_of_ocaml_toplevel_protocol.Worker_msg`

Types of the messages exchanged with a toplevel in a Web Worker.

```ocaml
module Message_id : sig ... end
```
```ocaml
module Fd : sig ... end
```
```ocaml
module Lexbuf : sig ... end
```
```ocaml
type step_result = [ 
  | `Phrase of bool
  | `Eof
 ]
```
```ocaml
type _ host_msg = 
  | Init : {
    cmis_base_url : string;
  } -> unit host_msg
  | Reset : unit host_msg
  | Check : {
    setenv : bool;
    code : string;
  } -> unit host_msg
  | Clear_check : unit host_msg
  | Execute : {
    code_fd : Fd.t option;
    print_outcome : bool;
    answer_fd : Fd.t;
    code : string;
  } -> bool host_msg
  | Use_string : {
    filename : string option;
    print_outcome : bool;
    answer_fd : Fd.t;
    code : string;
  } -> bool host_msg
  | Use_mod_string : {
    answer_fd : Fd.t;
    print_outcome : bool;
    modname : string;
    sig_code : string option;
    code : string;
  } -> bool host_msg
  | Import_scripts : string list -> unit host_msg
  | Open_lexbuf : {
    id : Lexbuf.t;
    code : string;
    code_fd : Fd.t option;
  } -> unit host_msg
  | Step : {
    lexbuf : Lexbuf.t;
    print_outcome : bool;
    answer_fd : Fd.t;
  } -> step_result host_msg
  | Close_lexbuf : {
    lexbuf : Lexbuf.t;
  } -> unit host_msg
```
```ocaml
type _ msg_ty = 
  | Unit : unit msg_ty
  | Bool : bool msg_ty
  | Int : int msg_ty
  | String : string msg_ty
  | Step_result : step_result msg_ty
```
```ocaml
type (_, _) eq = 
  | Eq : ('a, 'a) eq
```
```ocaml
type toploop_msg = 
  | Write : Fd.t * string -> toploop_msg
  | ReturnSuccess : Message_id.t
    * 'a msg_ty
    * 'a
    * Wrapped_intf.warning list -> toploop_msg
  | ReturnError : Message_id.t
    * Wrapped_intf.error
    * Wrapped_intf.warning list -> toploop_msg
```
```ocaml
val ty_of_host_msg : 't. 't host_msg -> 't msg_ty
```