
# Module type `Wrapped_intf.Wrapped`

```ocaml
type toplevel
```
```ocaml
type 'a result
```
```ocaml
type output
```
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