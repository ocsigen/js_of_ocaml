FROM ocaml/opam:debian-ocaml-5.3
WORKDIR /bench-dir

RUN sudo apt-get update \
 && sudo apt-get install -qq -yy --no-install-recommends \
      pkg-config libgmp-dev jq time

RUN sudo ln -sf /usr/bin/opam-2.3 /usr/bin/opam
RUN opam remote add origin https://github.com/ocaml/opam-repository.git --all --set-default \
 && opam update

# Build the benchmarks with Introcaml (OCaml 5.5 with introspection) to
# measure its impact. Override for a baseline, e.g.
#   docker build --build-arg OCAML_COMPILER=ocaml-base-compiler.5.5.0 ...
# Note: the Jane Street v0.18 preview packages (bonsai, ppx_template, ...)
# require ppxlib < 0.36 and thus OCaml < 5.4, so the partial render table
# benchmark is not built here and bin_prot comes from the v0.17 release.
ARG OCAML_COMPILER=ocaml-variants.5.5.0+introcaml1
RUN opam switch create bench "$OCAML_COMPILER" \
 && opam clean

# Install node
ENV NODE_VERSION=v24.0.0-v8-canary2025030537242e55ac
ENV NODE=node-$NODE_VERSION-linux-x64
RUN curl -q https://nodejs.org/download/v8-canary/$NODE_VERSION/$NODE.tar.xz \
  | tar xJf -
ENV PATH="/bench-dir/$NODE/bin:$PATH"

# Install binaryen
ENV BINARYEN_VERSION=version_122
ENV BINARYEN=binaryen-$BINARYEN_VERSION
RUN curl -Lq https://github.com/WebAssembly/binaryen/releases/download/$BINARYEN_VERSION/$BINARYEN-x86_64-linux.tar.gz \
  | tar zxf -
ENV PATH="/bench-dir/$BINARYEN/bin:$PATH"

# Install dependencies
WORKDIR /bench-dir/js_of_ocaml
COPY --chown=opam:opam js_of_ocaml-compiler.opam .
RUN opam install -y --deps-only ./js_of_ocaml-compiler.opam \
 && opam pin add -n bigstringaf https://github.com/ocaml-wasm/bigstringaf.git#wasm-latest \
 && opam install stringext uucp cstruct bigstringaf \
 && opam pin add ppxlib -n 0.38.0 \
 && opam clean

# Enable the wasm_of_ocaml backend in the dune-workspace for `make bench`.
ENV WASM_OF_OCAML=true

# Bin_prot packages (v0.17: no wasm runtime, see above)
RUN opam install ppx_bin_prot \
 && opam clean

# Copy sources
COPY --chown=opam:opam . ./

WORKDIR /bench-dir/js_of_ocaml/benchmarks
