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

# Run the test suites using bun in place of node (bun must be on PATH).
tests-bun:
	JSOO_ENGINE=bun dune build @runtest @runtest-js

# bun (JavaScriptCore) exposes the JSPI API but does not actually switch
# stacks, so the default (JSPI) effects mode cannot run there; use CPS effects.
tests-bun-wasm:
	JSOO_ENGINE=bun WASM_OF_OCAML=true dune build @runtest-wasm --profile with-effects

# Validates the Babel downleveling recipe documented in
# manual/browser-compat.wiki. Requires `npm install` at the repo root
# for @babel/cli, @babel/preset-env, core-js, and es-check.
test-babel-downlevel:
	JSOO_ROOT=$(CURDIR) dune build @runtest-babel-downlevel

# Regression test for #2300: MD5 of a >2 GiB input. Off by default because it
# digests a 2 GiB sparse file (~40s in JS), so it is not part of `make tests`.
test-md5-large:
	JSOO_TEST_MD5_LARGE=true dune build @runtest @runtest-js
	JSOO_TEST_MD5_LARGE=true WASM_OF_OCAML=true dune build @runtest-wasm

test runtest runtests: tests

# Build the API + manual (odoc) and the interactive examples. @doc compiles the
# manual/*.mld and the API into _build/default/_doc/_html/; @doc-manual builds the
# examples into _build/default/manual/files/. The themed ocsigen.org site is then
# produced by `wodoc build` (see doc/README.md), which the CI runs and deploys.
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

.PHONY: all tests tests-wasm tests-quickjs tests-bun tests-bun-wasm test runtest runtests doc clean bench
