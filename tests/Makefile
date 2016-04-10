include Makefile.common

TESTS_SRC=$(wildcard dir/*.ml)
TESTS_CMO=$(TESTS_SRC:.ml=.cmo)
TESTS_BYTE=$(TESTS_SRC:.ml=.byte)
TESTS_JS=$(TESTS_SRC:.ml=.js)
TESTS_LOG=$(TESTS_SRC:.ml=.log)

ifeq "${WITH_PPX}" "YES"
TESTS_PPX=$(patsubst ppx/%.ml,ppx/test_%,$(wildcard ppx/*.ml))
endif

TESTS_CAMLP4=$(patsubst camlp4/%.ml,camlp4/test_%,$(wildcard camlp4/*.ml))

PPX_DERIVING=$(shell opam config var lib)/ppx_deriving
COMMON=common.cmo

all: $(COMMON) $(TESTS_CMO) $(TESTS_BYTE) $(TESTS_JS) tests.byte $(TESTS_PPX) $(TESTS_CAMLP4)

test-ppx: $(TESTS_PPX)

test-camlp4: $(TESTS_CAMLP4)

phantom_check::
	@which phantomjs || (echo "You need phantomjs in your PATH to run this" && false)

clear_test::
	@rm -f dir/*.log dir/*.log.err

phantom: phantom_check clear_test all $(TESTS_LOG)

tests.byte: $(TESTS_CMO) $(COMMON)
	@$(OCAMLC) -linkpkg $(STDLIB) ../lib/deriving_json.cma common.cmo -package unix $(TESTS_CMO) -o tests.byte

%.log: %.js
	@(phantomjs $^ > $@ && head -n 1 $@) || (echo "Error for " $^ "\n" && mv $@ $@.err && cat $@.err)

clean:
	rm -f *.cm[io] *.byte *.js dir/*.cm[io] dir/*.byte dir/*.js dir/*.log
	rm -f ppx/*.cm[io] ppx/*.result
	rm -f camlp4/*.cm[io] camlp4/*.result

TEST_COMMON= -noprompt -I ../lib -I ../lib/deriving_json

ppx/%.result: ppx/%.ml
	TERM=dumb ocaml $(TEST_COMMON) -init ppx_init.ml -ppx ../lib/ppx/ppx_js -ppx "$(PPX_DERIVING)/ppx_deriving ../lib/ppx/ppx_deriving_json.cma" < $< 2>&1 | tail -n +15 > $@

ppx/test_%: ppx/%.result ppx/%.expected
	diff $?

camlp4/%.result: camlp4/%.ml
	TERM=dumb ocaml $(TEST_COMMON) -init camlp4_init.ml < $< 2>&1 | tail -n +20 > $@

camlp4/test_%: camlp4/%.result camlp4/%.expected
	diff $?
