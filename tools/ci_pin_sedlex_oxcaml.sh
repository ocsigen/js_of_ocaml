#!/bin/sh
# Pin sedlex 3.8.1 in an OxCaml switch.
#
# The OxCaml opam repository only has sedlex 3.3+ox, 3.6+ox and 3.7+ox,
# and js_of_ocaml-compiler requires sedlex >= 3.8.1. Pin the upstream
# 3.8.1 release, with the OxCaml patch of 3.7+ox ported to it, as
# sedlex.3.8.1+ox, and re-pin the oxcaml-sedlex-patches guard (part of
# the switch invariant) with this version added to the ones it accepts.
set -eu

patch_file="$(dirname "$0")/sedlex-3.8.1+ox.patch"

dir="$(mktemp -d)"
curl -sSL https://github.com/ocaml-community/sedlex/releases/download/v3.8.1/sedlex-3.8.1.tbz \
  | tar xj -C "$dir" --strip-components=1
patch -d "$dir" -p1 < "$patch_file"
opam pin add sedlex.3.8.1+ox "$dir" -k path -n -y

guard="$(mktemp -d)"
opam show --raw oxcaml-sedlex-patches \
  | perl -0777 -pe 's/= "3\.7\+ox"\)/= "3.7+ox" | = "3.8.1+ox")/' \
  > "$guard/oxcaml-sedlex-patches.opam"
grep -q '3.8.1+ox' "$guard/oxcaml-sedlex-patches.opam"
opam pin add oxcaml-sedlex-patches.enabled "$guard" -k path -n -y
