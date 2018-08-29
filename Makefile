VERSION := $(shell head -n 1 VERSION)

all:
	dune build @install @DEFAULT -j 8

tests:
	dune runtest

test runtest runtests: tests

doc: all
	$(MAKE) -C doc

promote:
	dune promote

toplevel-examples: all
	dune exec -- make -C toplevel/examples/lwt_toplevel_bin

clean:
	dune clean
	$(MAKE) -C toplevel/examples/lwt_toplevel_bin clean
	$(MAKE) -C doc clean

.PHONY: all tests test runtest runtests doc toplevel-examples clean remove-lexer-and-parser promote-lexer-and-parser
