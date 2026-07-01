
# Module `Js_of_ocaml_toplevel_common.Wrapped`

```ocaml
type loc = Js_of_ocaml_toplevel_protocol.Wrapped_intf.loc = {
  loc_start : int * int;
  loc_end : int * int;
}
```
The `result` types are defined in the dependency-free `msg` library and re-exported here.

```ocaml
type error = Js_of_ocaml_toplevel_protocol.Wrapped_intf.error = {
  msg : string;
  locs : loc list;
}
```
```ocaml
type warning = error
```
```ocaml
type 'a result = 'a Js_of_ocaml_toplevel_protocol.Wrapped_intf.result = 
  | Success of 'a * warning list
  | Error of error * warning list
```
```ocaml
val check : unit -> ?setenv:bool -> string -> unit result
```
Parse and typecheck a given source code

parameter setenv should the resulting environment replace the current environment? Note that setenv:true only type-checks: it adds definitions to the type environment without running them, so they have no runtime value. Code execution (execute, use, use\_mod\_string) is therefore rejected with an Error until the scratch environment is cleared (see Wrapped.clear\_check / Js\_of\_ocaml\_toplevel\_worker\_lwt\_client.clear\_check). check ~setenv:false (the default) does not touch the environment and stays freely interleavable with execution.
returns Success () in case of success and Error err where err contains the error message otherwise.
```ocaml
val execute : 
  unit ->
  ?ppf_code:Stdlib.Format.formatter ->
  ?print_outcome:bool ->
  ppf_answer:Stdlib.Format.formatter ->
  string ->
  bool result
```
Execute a given source code. The evaluation stops after the first toplevel phrase (as terminated by ";;") that fails to compile or for which the evaluation raises an uncaught exception.

parameter ppf\_code a formatter were the source code will be printed before its execution. The printing might be interleaved with call to "pp\_answer" when a line finishes by ";;".
parameter ppf\_answer a formatter were the compiler outputs will be printed.
parameter print\_outcome should the toplevel print the computed values and their types ?
returns Three outcomes:Success true: every phrase compiled and evaluated without raising.Success false: a phrase was compiled but raised an uncaught exception at evaluation time. The toplevel has already rendered the backtrace to ppf\_answer; no structured error is returned here.Error err: parsing or typechecking failed before any phrase could run; err.msg is the rendered error.
```ocaml
val use : 
  unit ->
  ?filename:string ->
  ?print_outcome:bool ->
  ppf_answer:Stdlib.Format.formatter ->
  string ->
  bool result
```
Execute a given source code. Unlike [`execute`](./#val-execute), the whole buffer is parsed up front (using `Toploop.parse_use_file`); each resulting phrase is then typechecked and evaluated in sequence. Parse errors abort before any phrase runs.

parameter filename a faked filename which will be used in error messages
parameter ppf\_answer see execute.
parameter print\_outcome whether to print the computed values and their types; unlike execute it defaults to false (silent, like Toploop.use\_silently).
returns as execute.
```ocaml
val use_mod_string : 
  unit ->
  ?print_outcome:bool ->
  ppf_answer:Stdlib.Format.formatter ->
  modname:string ->
  ?sig_code:string ->
  string ->
  bool result
```
Wrap a given source code into a module and bind it with a given name.

parameter print\_outcome see execute.
parameter ppf\_answer see execute.
parameter modname the module name, it must start with a capital character.
parameter sig\_code source code for the module signature.
returns as execute.
```ocaml
val clear_check : unit -> unit
```
Discard the scratch typing environment left by `check ~setenv:true`, restoring the environment as it was just before that call (and thus re-enabling [`execute`](./#val-execute), [`use`](./#val-use) and [`use_mod_string`](./#val-use_mod_string)). All definitions actually executed beforehand are kept; only the type-only definitions introduced by `check ~setenv:true` are dropped. A no-op when no scratch environment is active.

```ocaml
val make_lexbuf : 
  ?ppf_code:Stdlib.Format.formatter ->
  string ->
  Stdlib.Lexing.lexbuf
```
Build a normalized lexbuf for [`step`](./#val-step) from a source string. When `ppf_code` is given, the source is echoed to it as it is consumed. The lexbuf holds no resource beyond what the GC reclaims; there is nothing to close.

```ocaml
val step : 
  unit ->
  ?print_outcome:bool ->
  ppf_answer:Stdlib.Format.formatter ->
  Stdlib.Lexing.lexbuf ->
  [ `Phrase of bool | `Eof ] result
```
Parse, preprocess and evaluate the next toplevel phrase from the lexbuf. Returns `Success (`Phrase ok)` for a phrase that ran (`ok` is `false` if it raised at evaluation time, with the backtrace already on `ppf_answer`), `Success `Eof` once the buffer is exhausted, or `Error` on a parse/type error. The caller drives the loop and decides whether to continue after a failure.

```ocaml
val error_of_exn : exn -> error
```