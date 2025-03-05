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
    sudo cp node-v22.14.0-linux-x64/bin/node /usr/bin/node
COPY --chown=opam:opam ./*.opam ./VERSION ./
RUN opam pin -yn --with-version=dev .
RUN opam install -y --deps-only js_of_ocaml-compiler && \
    opam install -y binaryen-bin
COPY --chown=opam:opam . ./
RUN opam install -y wasm_of_ocaml-compiler
WORKDIR ./benchmarks
