
# Module `Js_of_ocaml_toplevel_worker`

Worker-side entrypoint.

```ocaml
val start : unit -> unit
```
Install the `onmessage` handler that drives the toplevel in this Web Worker, along with the stdout/stderr channel flushers and the `"cmis"` directive. Call once from the worker program's entry point; nothing is installed at module load.
