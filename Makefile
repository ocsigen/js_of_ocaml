all:
	dune build @all

tests:
	dune runtest

test runtest runtests: tests

doc:
	rm -rf doc-dev
	mkdir doc-dev
	dune clean
	dune build @doc --cache=disabled --force
	rsync -av --delete _build/default/_doc/_html/ doc-dev/api
	dune build @doc-manual --cache=disabled --force
	rsync -av --delete --exclude=".*" _build/default/manual/ doc-dev/manual
	find doc-dev/ -name dune -delete
	find doc-dev/ -name "*.exe" -delete

promote:
	dune promote

fmt:
	dune build @fmt --auto-promote 2> /dev/null || true
	git diff --exit-code

clean:
	dune clean

installdoc:
	git worktree add _wikidoc origin/wikidoc
	rsync -av doc-dev/ _wikidoc/doc/dev/

.PHONY: all tests test runtest runtests doc clean installdoc
