VERSION := $(shell head -n 1 VERSION)

all:
	jbuilder build @install @DEFAULT -j 8 --dev

tests:
	jbuilder runtest
test runtest runtests: tests

doc: all
	$(MAKE) -C doc

toplevel-examples: all
	jbuilder exec -- make -C toplevel/examples/lwt_toplevel_bin

clean:
	jbuilder clean
	$(MAKE) -C toplevel/examples/lwt_toplevel_bin clean
	$(MAKE) -C doc clean

remove-lexer-and-parser:
	rm compiler/lib/{annot_parser,js_parser}.{ml,mli}
promote-lexer-and-parser:
	cp  _build/default/compiler/lib/{annot_parser,js_parser}.{ml,mli} compiler/lib/

.PHONY: all tests test runtest runtests doc toplevel-examples clean remove-lexer-and-parser promote-lexer-and-parser
