FROM ocaml/opam:debian-ocaml-5.3
WORKDIR /bench-dir
RUN sudo apt-get update && \
    sudo apt-get install -qq -yy --no-install-recommends pkg-config libgmp-dev \
      wget
RUN sudo ln -sf /usr/bin/opam-2.3 /usr/bin/opam
RUN opam remote add origin https://github.com/ocaml/opam-repository.git && \
    opam update
RUN wget -q https://nodejs.org/download/v8-canary/v24.0.0-v8-canary2025030537242e55ac/node-v24.0.0-v8-canary2025030537242e55ac-linux-x64.tar.xz && \
    tar xf node-v24.0.0-v8-canary2025030537242e55ac-linux-x64.tar.xz
ENV PATH="/bench-dir/node-v24.0.0-v8-canary2025030537242e55ac-linux-x64/bin:$PATH"
RUN wget -q https://github.com/WebAssembly/binaryen/releases/download/version_122/binaryen-version_122-x86_64-linux.tar.gz && \
    tar xf binaryen-version_122-x86_64-linux.tar.gz
ENV PATH="/bench-dir/binaryen-version_122/bin:$PATH"
RUN opam install --fake binaryen-bin
COPY --chown=opam:opam ./*.opam ./
RUN opam pin -yn --with-version=dev .
RUN opam install -y --deps-only js_of_ocaml-compiler
COPY --chown=opam:opam . ./
RUN opam install -y wasm_of_ocaml-compiler
WORKDIR ./benchmarks
