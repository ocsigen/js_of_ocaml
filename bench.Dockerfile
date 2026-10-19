FROM ocaml/opam:debian-ocaml-5.5
WORKDIR /bench-dir

RUN sudo apt-get update \
 && sudo apt-get install -qq -yy --no-install-recommends \
      pkg-config libgmp-dev zlib1g-dev jq time

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
ENV RELOADED_TAG=v0.18_preview.130.106+341+reloaded
RUN git clone https://github.com/public-release-reloaded/opam-repository reloaded \
 && git -C reloaded checkout c5daed22034c1806f9f307d67bf8d899cad0903f \
 && opam repository add reloaded ./reloaded

# Install dependencies
WORKDIR /bench-dir/js_of_ocaml
COPY --chown=opam:opam js_of_ocaml-compiler.opam .
RUN opam install -y --deps-only ./js_of_ocaml-compiler.opam \
 && opam clean

# Real-world programs: query-json, herd (herdtools7) and Alt-Ergo. Installed
# first since Alt-Ergo (through dolmen) requires an older menhir, which
# js_of_ocaml and the Jane Street packages below then use as well.
RUN opam install -y --deps-only query-json.0.6.1 herdtools7.7.58 alt-ergo.2.6.4 \
 && opam clean

# Enable the wasm_of_ocaml backend in the top-level dune-workspace, for
# `make bench`
ENV WASM_OF_OCAML=true

# Bigstringaf with Wasm support (not released yet, 0.10.0 lacks it), for
# Camlboy and the Jane Street packages
RUN opam pin add -n bigstringaf https://github.com/inhabitedtype/bigstringaf.git#master

# Partial Render Table and bin_prot packages, and Zarith's stubs (with Wasm
# support since v0.18) for herd and Alt-Ergo. The browser package needs a
# patch for the parameterized Dom_html.messageEvent of js_of_ocaml > 6.3.0
# (https://github.com/janestreet/browser/pull/1), hence a recent
# js_of_ocaml; `make bench` then rebuilds them with the current sources.
COPY --chown=opam:opam benchmarks/benchmark-partial-render-table/patches ./prt-patches
RUN opam pin add -yn --with-version=dev https://github.com/ocsigen/js_of_ocaml.git#15b280e880a46450b8abaa033d43bcbb722bbef4 \
 && git clone --depth 1 -b $RELOADED_TAG https://github.com/public-release-reloaded/browser.git ../browser \
 && git -C ../browser apply $PWD/prt-patches/browser.patch \
 && opam pin add -yn --kind=path browser.$(echo $RELOADED_TAG | sed 's/_/~/') ../browser \
 && git clone --depth 1 -b $RELOADED_TAG https://github.com/public-release-reloaded/bonsai_web_components.git ../bonsai_web_components \
 && git -C ../bonsai_web_components apply $PWD/prt-patches/bonsai_web_components.patch \
 && opam install -y --deps-only ../bonsai_web_components/bonsai_web_components.opam \
 && opam install -y bonsai_bench core_bench incr_map ppx_bin_prot zarith_stubs_js \
 && (opam exec -- dune build --root ../bonsai_web_components --profile release partial_render_table/bench/bin/main.bc-for-jsoo || true) \
 && opam clean

# Digestif (pure OCaml hash functions)
RUN opam install digestif

# Camlboy
RUN opam install -y bigstringaf ppx_deriving

# Copy sources
COPY --chown=opam:opam . ./

WORKDIR /bench-dir/js_of_ocaml/benchmarks
