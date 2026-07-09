// Dummy runtime file. Its presence makes dune build a per-executable
// standalone runtime instead of the shared one, so that the
// build_runtime_flags --file embeds are honored. Works around
// ocaml/dune#15455; remove once the fix is released.
