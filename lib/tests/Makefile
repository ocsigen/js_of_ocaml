include Makefile.common

TESTS_SRC=$(wildcard dir/*.ml)
TESTS_CMO=$(TESTS_SRC:.ml=.cmo)
COMMON=common.cmo

all: tests.js

check::
	@which node || (echo "You need node in your PATH to run this" && false)

clear_test::
	@rm -f dir/*.log dir/*.log.err

test-with-node: check all
	node tests.js

%.cmo: %.ml

tests.byte: $(TESTS_CMO) $(COMMON)
	$(OCAMLC) -linkpkg $(STDLIB) ../../lib/deriving_json.cma common.cmo -package unix $(TESTS_CMO) -o tests.byte

clean:
	rm -f *.cm[io] *.byte *.js dir/*.cm[io] dir/*.byte dir/*.js dir/*.log
