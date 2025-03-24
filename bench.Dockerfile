FROM ocaml/opam:debian-ocaml-5.2
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
RUN opam install --fake binaryen-bin

# Jane Street opam packages
RUN mkdir janestreet \
 && cd janestreet \
 && git clone --depth 20 https://github.com/janestreet/opam-repository \
 && cd opam-repository \
 && git checkout 41c89c7824533f6b63cc5b6d75e6ddb1441d1520 \
 && opam remote add js .

# Install dependencies
COPY --chown=opam:opam ./*.opam ./
RUN opam pin -yn --with-version=dev .
RUN opam install -y --deps-only js_of_ocaml-compiler

# Install js_of_ocaml / wasm_of_ocaml
COPY --chown=opam:opam . ./
RUN opam install -y wasm_of_ocaml-compiler

# Compile partial render table benchmark
RUN opam install opam-format stringext uucp cstruct
RUN opam exec -- dune exec tools/ci_setup.exe janestreet .
RUN opam install ppxlib.0.35.0 # temporary workaround
RUN cd janestreet/lib/bonsai_web_components && git config pull.rebase true && git pull
RUN eval $(opam env) \
 && dune build --root janestreet --profile release lib/bonsai_web_components/partial_render_table/bench/bin/main.bc.wasm.js lib/bonsai_web_components/partial_render_table/bench/bin/main.bc.js
RUN cp -r janestreet/_build/default/lib/bonsai_web_components/partial_render_table/bench/bin/main.bc.* ./benchmarks/benchmark-partial-render-table

WORKDIR ./benchmarks
