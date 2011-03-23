
all: check_lwt compiler library runtime examples

include Makefile.conf

.PHONY: compiler library runtime examples check_lwt

compiler:
	$(MAKE) -C compiler
library:
	$(MAKE) -C lib
runtime:
	$(MAKE) -C runtime
toplevel: compiler library runtime
	$(MAKE) -C toplevel
examples: compiler library runtime
	$(MAKE) -C examples
tests: compiler library
	$(MAKE) -C tests

LWTERROR="Js_of_ocaml requires Lwt version 2.2.1 at least.  Please upgrade."
check_lwt:
	@if ocamlfind query lwt -l | ocaml tools/check_version.ml 2.2.1; then \
	  echo $(LWTERROR); exit 1; \
	fi

ifneq "${DERIVING}" ""
JSON_DERIVING=lib/syntax/pa_deriving_Json.cmo lib/deriving_json.cma lib/deriving_json.cmxa lib/deriving_json.cmxs lib/deriving_json.a lib/deriving_json/deriving_Json.cmi lib/deriving_json/deriving_Json.cmi lib/deriving_json/deriving_Json_lexer.cmi  lib/deriving_json/deriving_bi_outbuf.cmi
endif

install:
	ocamlfind install $(LIBRARY) lib/META lib/$(LIBNAME).cma lib/dll$(LIBNAME).so lib/lib$(LIBNAME).a lib/*.cmi lib/*.mli lib/syntax/pa_js.cmo ${JSON_DERIVING} runtime/runtime.js
	install compiler/$(COMPILER) $(BINDIR)

uninstall:
	ocamlfind remove $(LIBRARY)
	rm -f $(BINDIR)/$(COMPILER)

depend:
	$(MAKE) -C compiler depend
	$(MAKE) -C lib depend

clean:
	$(MAKE) -C compiler clean
	$(MAKE) -C lib clean
	$(MAKE) -C runtime clean
	$(MAKE) -C examples clean
ifeq ($(wildcard tests),tests)
	$(MAKE) -C tests clean
endif

realclean: clean
	find . -name "*~" -print | xargs rm -f

dist:
	rm -r /tmp/js_of_ocaml &&\
        cd /tmp &&\
	darcs get http://ocsigen.org/darcs/js_of_ocaml/ &&\
	tar zcvf js_of_ocaml.tar.gz js_of_ocaml --exclude benchmarks --exclude _darcs --exclude tests
