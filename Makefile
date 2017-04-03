VERSION := $(shell head -n 1 VERSION)

all:
	jbuilder build @install @DEFAULT -j 8

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

.PHONY: all doc clean toplevel-examples tests remove-lexer-and-parser promote-lexer-and-parser
