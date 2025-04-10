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
WORKDIR /bench-dir/js_of_ocaml
COPY --chown=opam:opam js_of_ocaml-compiler.opam .
RUN opam install -y --deps-only ./js_of_ocaml-compiler.opam \
 && opam install opam-format stringext uucp cstruct \
 && opam clean

# Prepare partial render table benchmark
COPY --chown=opam:opam dune-project ./
COPY --chown=opam:opam tools ./tools
RUN opam exec -- dune exec tools/ci_setup.exe ../janestreet . \
 && opam exec -- dune build --root ../janestreet --profile release lib/bonsai_web_components/partial_render_table/bench/bin/main.bc-for-jsoo \
 && opam remove js_of_ocaml-compiler ojs \
 && opam clean

# Copy sources
COPY --chown=opam:opam . ./

WORKDIR /bench-dir/js_of_ocaml/benchmarks
