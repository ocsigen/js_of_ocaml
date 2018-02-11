VERSION := $(shell head -n 1 VERSION)

all:
	jbuilder build @install @DEFAULT -j 8 --dev

tests:
	jbuilder runtest --dev

test runtest runtests: tests

doc: all
	$(MAKE) -C doc

promote:
	jbuilder promote

toplevel-examples: all
	jbuilder exec -- make -C toplevel/examples/lwt_toplevel_bin

clean:
	jbuilder clean
	$(MAKE) -C toplevel/examples/lwt_toplevel_bin clean
	$(MAKE) -C doc clean

.PHONY: all tests test runtest runtests doc toplevel-examples clean remove-lexer-and-parser promote-lexer-and-parser
