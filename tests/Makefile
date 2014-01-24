include Makefile.common

TESTS_SRC=$(wildcard dir/*.ml)
TESTS_CMO=$(TESTS_SRC:.ml=.cmo)
TESTS_BYTE=$(TESTS_SRC:.ml=.byte)
TESTS_JS=$(TESTS_SRC:.ml=.js)
TESTS_LOG=$(TESTS_SRC:.ml=.log)

COMMON=common.cmo

all: $(COMMON) $(TESTS_CMO) $(TESTS_BYTE) $(TESTS_JS) tests.byte

phantom_check::
	@which phantomjs || (echo "You need phantomjs in your PATH to run this" && false)

clear_test::
	@rm -f dir/*.log dir/*.log.err

phantom: phantom_check clear_test all $(TESTS_LOG)

tests.byte: $(TESTS_CMO) $(COMMON)
	@$(OCAMLC) -linkpkg $(STDLIB) ../lib/deriving_json.cma common.cmo -package unix $(TESTS_CMO) -o tests.byte

%.log: %.js
	(phantomjs $^ > $@ && head -n 1 $@) || (echo "Error for " $^ "\n" && mv $@ $@.err && cat $@.err)

clean:
	rm -f *.cm[io] *.byte *.js dir/*.cm[io] dir/*.byte dir/*.js dir/*.log
