
# Module `Js_of_ocaml_toplevel_worker_lwt_client`

```ocaml
type toplevel
```
```ocaml
type 'a result = 'a Js_of_ocaml_toplevel_protocol.Wrapped_intf.result Lwt.t
```
```ocaml
type output = string -> unit
```
```ocaml
type lexbuf
```
Handle for a worker-side parsing session created by [`open_lexbuf`](./#val-open_lexbuf) and stepped one phrase at a time with [`step`](./#val-step).

```ocaml
exception Init_failed of Js_of_ocaml_toplevel_protocol.Wrapped_intf.error
```
Raised (as the result of the [`create`](./#val-create) thread) when the worker fails to initialize its toplevel environment — for instance when the worker script cannot be loaded or `stdlib.cmis.js` is missing under the `cmis_base_url`.

```ocaml
val create : 
  ?cmis_base_url:string ->
  ?after_init:(toplevel -> unit Lwt.t) ->
  pp_stdout:output ->
  pp_stderr:output ->
  js_file:string ->
  unit ->
  toplevel Lwt.t
```
Spawn the worker and initialize its toplevel. The returned thread fails with [`Init_failed`](./#exception-Init_failed) if initialization does not succeed.

The worker fetches `<cmis_base_url>stdlib.cmis.js` at startup only if the cmis are not already embedded (i.e. it was built with `--no-cmis`).

```ocaml
val set_after_init : toplevel -> (toplevel -> unit Lwt.t) -> unit
```
```ocaml
val import_cmis_js : 
  toplevel ->
  string ->
  unit Js_of_ocaml_toplevel_protocol.Wrapped_intf.result Lwt.t
```
```ocaml
val reset : toplevel -> ?timeout:(unit -> unit Lwt.t) -> unit -> unit Lwt.t
```
```ocaml
val interrupt : toplevel -> unit Lwt.t
```
Forcibly terminate the worker — use this when a computation is stuck in an infinite loop and so cannot be reached by [`reset`](./#val-reset)'s cooperative `Reset` message — and respawn a fresh, re-initialized one (re-importing the cmis bundles and running `after_init`). Any request in flight fails with `Lwt.Canceled`. All toplevel state is lost, since the worker process is killed.

```ocaml
val clear_check : toplevel -> unit Lwt.t
```
Discard the scratch typing environment left by `check ~setenv:true` on the worker, re-enabling [`execute`](./#val-execute) and the other code-running operations. See [`Js_of_ocaml_toplevel.Wrapped.clear_check`](./Js_of_ocaml_toplevel_common-Wrapped.md#val-clear_check).

```ocaml
val open_lexbuf : toplevel -> ?ppf_code:output -> string -> lexbuf Lwt.t
```
Open a worker-side parsing session over `code` to be stepped with [`step`](./#val-step). When `ppf_code` is given, the source is echoed to it as it is consumed. Release it with [`close_lexbuf`](./#val-close_lexbuf).

```ocaml
val step : 
  toplevel ->
  ?print_outcome:bool ->
  ppf_answer:output ->
  lexbuf ->
  [ `Phrase of bool | `Eof ] result
```
Evaluate the next phrase of a session. ``Phrase ok` ran a phrase (`ok` is `false` if it raised at evaluation), ``Eof` once the buffer is exhausted, and an `Error` result on a parse/type error. The caller drives the loop and decides whether to continue after a failure.

```ocaml
val close_lexbuf : toplevel -> lexbuf -> unit Lwt.t
```
Release a session opened with [`open_lexbuf`](./#val-open_lexbuf).

```ocaml
val check : toplevel -> ?setenv:bool -> string -> unit result
```
Parse and typecheck a given source code

parameter setenv should the resulting environment replace the current environment? Note that setenv:true only type-checks: it adds definitions to the type environment without running them, so they have no runtime value. Code execution (execute, use, use\_mod\_string) is therefore rejected with an Error until the scratch environment is cleared (see Wrapped.clear\_check / Js\_of\_ocaml\_toplevel\_worker\_lwt\_client.clear\_check). check ~setenv:false (the default) does not touch the environment and stays freely interleavable with execution.
returns Success () in case of success and Error err where err contains the error message otherwise.
```ocaml
val execute : 
  toplevel ->
  ?ppf_code:output ->
  ?print_outcome:bool ->
  ppf_answer:output ->
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
  toplevel ->
  ?filename:string ->
  ?print_outcome:bool ->
  ppf_answer:output ->
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
  toplevel ->
  ?print_outcome:bool ->
  ppf_answer:output ->
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