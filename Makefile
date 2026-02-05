all:
	dune build @all

tests:
	dune build @runtest @runtest-js

tests-wasm:
	WASM_OF_OCAML=true dune build @runtest-wasm

test runtest runtests: tests

doc:
	rm -rf doc-dev
	mkdir doc-dev
	dune build @doc @doc-manual
	rsync -av --delete _build/default/_doc/_html/ doc-dev/api
	rsync -av --delete --exclude=".*" --exclude="dune*" --exclude="gen_dune_inc.*" _build/default/manual/ doc-dev/manual
	find doc-dev/ -name dune -delete
	find doc-dev/ -name "*.exe" -delete

promote:
	dune promote

fmt:
	dune build @fmt --auto-promote 2> /dev/null || true
	git diff --exit-code

fmt-js:
	npx @biomejs/biome@1.9 format --write

lint-js:
	npx @biomejs/biome@1.9 lint

clean:
	dune clean

installdoc:
	if [ -d _wikidoc ]; then git fetch origin wikidoc && git -C _wikidoc checkout origin/wikidoc; else git worktree add _wikidoc origin/wikidoc; fi
	rsync -av doc-dev/ _wikidoc/doc/dev/

bench:
	$(MAKE) -C benchmarks bench

.PHONY: all tests tests-wasm test runtest runtests doc clean installdoc bench
