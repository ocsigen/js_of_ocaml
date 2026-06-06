all:
	dune build @all

tests:
	dune build @runtest @runtest-js

tests-wasm:
	WASM_OF_OCAML=true dune build @runtest-wasm

# Run the JS test suite using QuickJS-NG (`qjs`) in place of `node`.
# Override the engine binary with JSOO_QUICKJS_BIN if `qjs` is not on PATH.
tests-quickjs:
	dune build @runtest @runtest-js --profile=quickjs

# Validates the Babel downleveling recipe documented in
# manual/browser-compat.wiki. Requires `npm install` at the repo root
# for @babel/cli, @babel/preset-env, core-js, and es-check.
test-babel-downlevel:
	JSOO_ROOT=$(CURDIR) dune build @runtest-babel-downlevel

test runtest runtests: tests

# Build the API + manual (odoc) and the interactive examples. @doc compiles the
# manual/*.mld and the API into _build/default/_doc/_html/; @doc-manual builds the
# examples into _build/default/manual/files/. The themed ocsigen.org site is then
# produced by doc/build.sh (see doc/README.md), which the CI runs and deploys.
doc:
	dune build @doc @doc-manual

promote:
	dune promote

fmt:
	dune build @fmt --auto-promote 2> /dev/null || true
	git diff --exit-code

fmt-js:
	npx @biomejs/biome format --write

lint-js:
	npx @biomejs/biome lint

clean:
	dune clean

bench:
	$(MAKE) -C benchmarks bench

.PHONY: all tests tests-wasm tests-quickjs test runtest runtests doc clean bench
