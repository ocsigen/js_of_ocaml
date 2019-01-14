VERSION := $(shell head -n 1 VERSION)

all:
	dune build @install @default -j 8

tests:
	dune runtest

test runtest runtests: tests

doc: all
	$(MAKE) -C doc

promote:
	dune promote

clean:
	dune clean
	$(MAKE) -C doc clean

.PHONY: all tests test runtest runtests doc clean
