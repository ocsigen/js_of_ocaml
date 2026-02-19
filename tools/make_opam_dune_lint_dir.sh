#!/bin/sh
# Define empty libraries stdlib_stable and stdlib_upstream_compatible
# to keep opam-dune-lint happy (these libraries are OxCaml-specific).

mkdir opam_dune_lint_dir && cd opam_dune_lint_dir
echo "(lang dune 3.20)" > dune-project
touch stdlib_stable.opam
touch stdlib_upstream_compatible.opam
cat > dune <<EOF
(library (public_name stdlib_stable))
(library (public_name stdlib_upstream_compatible))
EOF
