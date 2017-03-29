include Makefile.conf

VERSION := $(shell head -n 1 VERSION)

all:
	jbuilder build @install @DEFAULT -j 8

tests: all
	$(MAKE) -C lib/tests test-with-node
	$(MAKE) -C camlp4/pa_js tests
	$(MAKE) -C camlp4/pa_deriving_json tests
	$(MAKE) -C ppx/ppx_js tests
	$(MAKE) -C ppx/ppx_deriving_json tests

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

.PHONY: all doc clean toplevel-examples tests dist

dist:
	rm -rf /tmp/js_of_ocaml-${VERSION} &&\
        cd /tmp &&\
	git clone https://github.com/ocsigen/js_of_ocaml.git js_of_ocaml-${VERSION} &&\
	(cd js_of_ocaml-${VERSION}; git checkout ${VERSION}) &&\
	tar zcvf js_of_ocaml-${VERSION}.tar.gz js_of_ocaml-${VERSION} --exclude benchmarks --exclude .git
