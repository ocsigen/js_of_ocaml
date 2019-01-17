all:
	dune build @install @default -j 8

tests:
	dune runtest

test runtest runtests: tests

doc:
	dune build @ocsigen-doc

promote:
	dune promote

clean:
	dune clean

.PHONY: all tests test runtest runtests doc clean
