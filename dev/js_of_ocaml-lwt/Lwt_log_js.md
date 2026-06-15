
# Module `Lwt_log_js`

```ocaml
type level = Lwt_log_core.level = 
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Fatal
```
```ocaml
type logger = Lwt_log_core.logger
```
```ocaml
type section = Lwt_log_core.section
```
```ocaml
val string_of_level : level -> string
```
```ocaml
val level_of_string : string -> level option
```
```ocaml
val load_rules : ?fail_on_error:bool -> string -> unit
```
```ocaml
val add_rule : string -> level -> unit
```
```ocaml
val append_rule : string -> level -> unit
```
```ocaml
val reset_rules : unit -> unit
```
```ocaml
module Section : sig ... end
```
```ocaml
type template = Lwt_log_core.template
```
```ocaml
val render : 
  buffer:Stdlib.Buffer.t ->
  template:template ->
  section:section ->
  level:level ->
  message:string ->
  unit
```
```ocaml
val location_key : (string * int * int) Lwt.key
```
```ocaml
exception Logger_closed
```
```ocaml
val make : 
  output:(section -> level -> string list -> unit Lwt.t) ->
  close:(unit -> unit Lwt.t) ->
  logger
```
```ocaml
val close : logger -> unit Lwt.t
```
```ocaml
val default : logger Stdlib.ref
```
```ocaml
val broadcast : logger list -> logger
```
```ocaml
val dispatch : (section -> level -> logger) -> logger
```
```ocaml
val null : logger
```
Lwt logger for js\_of\_ocaml


### Predefined logger

```ocaml
val console : Lwt_log_core.logger
```
Logger that use the javascript console object.


### Logging functions

```ocaml
val log : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  level:level ->
  string ->
  unit Lwt.t
```
`log ?section ?logger ~level message` logs a message.

`section` defaults to [`Section.main`](./Lwt_log_js-Section.md#val-main). If `logger` is not specified, then the default one is used instead (see [`default`](./#val-default)).

If `exn` is provided, then its string representation (= `Printexc.to_string exn`) will be append to the message, and if possible the backtrace will also be logged.

If `inspect` is provided, it will be append to the message.

`location` contains the location of the logging directive, it is of the form `(file_name, line, column)`.

```ocaml
val log_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  level:level ->
  ('a, unit, string, unit Lwt.t) Stdlib.format4 ->
  'a
```
`log_f` is the same as `log` except that it takes a format string

```ocaml
val ign_log : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  level:level ->
  string ->
  unit
```
Same as [`log`](./#val-log) but ignore the resulting thread.

```ocaml
val ign_log_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  level:level ->
  ('a, unit, string, unit) Stdlib.format4 ->
  'a
```
Same as [`log_f`](./#val-log_f) but ignore the resulting thread.

The following functions are the same as [`log`](./#val-log) except that their name determines which level is used.

For example `info msg` is the same as `log ~level:Info msg`.

```ocaml
val debug : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit Lwt.t
```
```ocaml
val debug_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit Lwt.t) Stdlib.format4 ->
  'a
```
```ocaml
val ign_debug : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit
```
```ocaml
val ign_debug_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit) Stdlib.format4 ->
  'a
```
```ocaml
val info : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit Lwt.t
```
```ocaml
val info_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit Lwt.t) Stdlib.format4 ->
  'a
```
```ocaml
val ign_info : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit
```
```ocaml
val ign_info_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit) Stdlib.format4 ->
  'a
```
```ocaml
val notice : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit Lwt.t
```
```ocaml
val notice_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit Lwt.t) Stdlib.format4 ->
  'a
```
```ocaml
val ign_notice : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit
```
```ocaml
val ign_notice_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit) Stdlib.format4 ->
  'a
```
```ocaml
val warning : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit Lwt.t
```
```ocaml
val warning_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit Lwt.t) Stdlib.format4 ->
  'a
```
```ocaml
val ign_warning : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit
```
```ocaml
val ign_warning_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit) Stdlib.format4 ->
  'a
```
```ocaml
val error : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit Lwt.t
```
```ocaml
val error_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit Lwt.t) Stdlib.format4 ->
  'a
```
```ocaml
val ign_error : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit
```
```ocaml
val ign_error_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit) Stdlib.format4 ->
  'a
```
```ocaml
val fatal : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit Lwt.t
```
```ocaml
val fatal_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit Lwt.t) Stdlib.format4 ->
  'a
```
```ocaml
val ign_fatal : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  string ->
  unit
```
```ocaml
val ign_fatal_f : 
  ?inspect:'v ->
  ?exn:exn ->
  ?section:section ->
  ?location:(string * int * int) ->
  ?logger:logger ->
  ('a, unit, string, unit) Stdlib.format4 ->
  'a
```