VERSION := $(shell head -n 1 VERSION)

all:
	jbuilder build @install @DEFAULT -j 8 --dev

tests:
	jbuilder runtest

doc: all
	$(MAKE) -C doc

toplevel-examples: all
	jbuilder exec -- make -C toplevel/examples/lwt_toplevel_bin

clean:
	rm -rf _build
	$(MAKE) -C toplevel/examples/lwt_toplevel_bin clean
	$(MAKE) -C doc clean

remove-lexer-and-parser:
	rm compiler/lib/{annot_parser,js_parser}.{ml,mli}
promote-lexer-and-parser:
	cp  _build/default/compiler/lib/{annot_parser,js_parser}.{ml,mli} compiler/lib/

.PHONY: all tests doc toplevel-examples clean remove-lexer-and-parser promote-lexer-and-parser
