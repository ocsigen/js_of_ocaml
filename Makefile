all:
	dune build @install @default -j 8

tests:
	dune runtest

test runtest runtests: tests

doc:
	dune build @ocsigen-doc

promote:
	dune promote

fmt:
	dune build @fmt --auto-promote 2> /dev/null || true
	git diff --exit-code

clean:
	dune clean

installdoc:
	rm -rf _wikidoc
	git clone ./ _wikidoc
	(cd _wikidoc && git checkout wikidoc)
	rm -rf _wikidoc/doc/dev/*
	cp -r _build/default/_doc/_html _wikidoc/doc/dev/api
	cp -r _build/default/manual _wikidoc/doc/dev/manual
	find _wikidoc/doc/dev/ -name dune -delete

.PHONY: all tests test runtest runtests doc clean installdoc
