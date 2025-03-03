FROM ocaml/opam:debian-ocaml-5.3
WORKDIR /bench-dir
RUN sudo apt-get update
RUN sudo apt-get install -qq -yy --no-install-recommends pkg-config libgmp-dev
RUN sudo ln -sf /usr/bin/opam-2.1 /usr/bin/opam
COPY --chown=opam:opam . ./
RUN opam remote add origin https://github.com/ocaml/opam-repository.git && \
    opam update
RUN opam exec -- git init
RUN opam exec -- git add VERSION
RUN opam exec -- git commit -m dummy
RUN opam exec -- git tag -a $(cat VERSION)-dev -m dummy
RUN opam pin -yn --with-version=dev .
RUN opam exec -- opam install -y --depext-only ./wasm_of_ocaml-bench.opam
RUN opam install -y --deps-only --with-test ./wasm_of_ocaml-bench.opam
WORKDIR ./benchmarks
RUN make bench
