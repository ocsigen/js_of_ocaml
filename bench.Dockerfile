FROM ocaml/opam:debian-ocaml-5.5
WORKDIR /bench-dir

RUN sudo apt-get update \
 && sudo apt-get install -qq -yy --no-install-recommends \
      pkg-config libgmp-dev jq time

RUN sudo ln -sf /usr/bin/opam-2.3 /usr/bin/opam
RUN opam remote add origin https://github.com/ocaml/opam-repository.git \
 && opam update

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

# Jane Street packages for OCaml 5.5 (Partial Render Table, bin_prot):
# Jane Street's own preview packages require ppxlib < 0.36, hence
# OCaml <= 5.3, so use the "public release reloaded" repository instead
# (https://discuss.ocaml.org/t/jane-street-libraries-base-core-async-etc-with-5-5/18469)
RUN mkdir janestreet \
 && git clone https://github.com/public-release-reloaded/opam-repository janestreet/opam-repository \
 && git -C janestreet/opam-repository checkout c5daed22034c1806f9f307d67bf8d899cad0903f \
 && opam repository add reloaded ./janestreet/opam-repository

# Install dependencies
WORKDIR /bench-dir/js_of_ocaml
COPY --chown=opam:opam js_of_ocaml-compiler.opam .
RUN opam install -y --deps-only ./js_of_ocaml-compiler.opam \
 && opam install opam-format stringext uucp cstruct \
 && opam clean

# Enable the js_of_ocaml and wasm_of_ocaml backends in the
# ci_setup-generated dune-workspace, both for the bonsai bench build below
# and for `make bench` later on.
ENV JS_OF_OCAML=true
ENV WASM_OF_OCAML=true

# Enable the build of tools/ci_setup.exe (needs opam-format)
ENV BUILD_CI_SETUP=true

# Prepare partial render table benchmark. The Jane Street sources are
# cloned into ../janestreet and built there, so that `make bench` only has
# to rebuild what depends on js_of_ocaml. The browser package needs the
# parameterized Dom_html.messageEvent of js_of_ocaml > 6.3.0, hence a
# recent js_of_ocaml for the initial build.
COPY --chown=opam:opam dune-project ./
COPY --chown=opam:opam tools ./tools
COPY --chown=opam:opam benchmarks/benchmark-partial-render-table/bonsai_web_components.patch ./
RUN opam pin add -yn --with-version=dev https://github.com/ocsigen/js_of_ocaml.git#15b280e880a46450b8abaa033d43bcbb722bbef4 \
 && opam exec -- dune exec tools/ci_setup.exe ../janestreet . \
 && git -C ../janestreet/lib/bonsai_web_components apply $PWD/bonsai_web_components.patch \
 && (opam exec -- dune build --root ../janestreet --profile release lib/bonsai_web_components/partial_render_table/bench/bin/main.bc-for-jsoo || true) \
 && opam remove -y js_of_ocaml-compiler ojs \
 && opam clean

# Bin_prot packages
RUN opam install -y ppx_bin_prot

# Digestif (pure OCaml hash functions)
RUN opam install digestif

# Camlboy
RUN opam install -y bigstringaf ppx_deriving

# Copy sources
COPY --chown=opam:opam . ./

WORKDIR /bench-dir/js_of_ocaml/benchmarks
