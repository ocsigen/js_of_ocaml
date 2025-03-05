FROM ocaml/opam:debian-ocaml-5.3
WORKDIR /bench-dir
RUN sudo apt-get update && \
    sudo apt-get install -qq -yy --no-install-recommends pkg-config libgmp-dev \
      wget
RUN sudo ln -sf /usr/bin/opam-2.3 /usr/bin/opam
RUN opam remote add origin https://github.com/ocaml/opam-repository.git && \
    opam update
RUN wget https://nodejs.org/dist/v22.14.0/node-v22.14.0-linux-x64.tar.xz && \
    tar xJvf node-v22.14.0-linux-x64.tar.xz && \
    sudo cp node-v22.14.0-linux-x64/bin/node /usr/bin/node && \
    wget https://github.com/WebAssembly/binaryen/releases/download/version_122/binaryen-version_122-x86_64-linux.tar.gz && \
    tar xzvf binaryen-version_122-x86_64-linux.tar.gz && \
    sudo cp binaryen-version_122/bin/wasm-opt binaryen-version_122/bin/wasm-metadce binaryen-version_122/bin/wasm-merge /usr/bin/
COPY --chown=opam:opam ./*.opam ./VERSION ./
RUN opam pin -yn --kind=path --with-version=dev .
RUN opam install -y --deps-only js_of_ocaml-compiler
COPY --chown=opam:opam . ./
RUN opam install -y wasm_of_ocaml-compiler
WORKDIR ./benchmarks
