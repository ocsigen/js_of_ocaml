.PHONY: bench perform

# Alt-Ergo, an SMT solver (https://github.com/OCamlPro/alt-ergo), using its
# Node.js entry point: arbitrary-precision arithmetic through
# zarith_stubs_js, hash-consing, backtracking search.
# node.patch disables the Unix timers, which are not available in JavaScript
# (upstream's JavaScript build disables them the same way).

export NAME=Alt-Ergo

SHELL=/bin/bash -o pipefail

VERSION=v2.6.4
DIR=alt-ergo
BC=$(DIR)/_build/default/src/bin/js/main_text_js.bc
ZARITH=$(shell opam var zarith_stubs_js:lib)/runtime.js
ZARITH_WAT=$(shell opam var zarith_stubs_js:lib)/runtime.wat

bench: $(BC)
	@date -u +"%FT%TZ - $(NAME): starting"
	$(MAKE) perform COMPILER=js_of_ocaml SCRIPT=alt-ergo.js KIND=js STUBS="$(ZARITH) +toplevel.js +dynlink.js"
	$(MAKE) perform COMPILER=wasm_of_ocaml SCRIPT=alt-ergo.wasm.js KIND=wasm STUBS="$(ZARITH) $(ZARITH_WAT)"
	@date -u +"%FT%TZ - $(NAME): done"

$(BC):
	git clone --depth 1 -b $(VERSION) https://github.com/OCamlPro/alt-ergo $(DIR)
	cd $(DIR) && patch -p1 < ../node.patch
	dune build --root $(DIR) --profile release src/bin/js/main_text_js.bc

# The slowest problems of the test suite
PROBLEMS=$(wildcard $(DIR)/tests/slow/issues/505/*.ae)

perform:
	/usr/bin/time -f "%E %M" $(COMPILER) --debug times $(STUBS) $(BC) -o $(SCRIPT) 2>&1 | \
	ocaml -I +str str.cma ../utils/compilation_metrics.ml $(COMPILER) $(NAME) $(SCRIPT) | \
	sh ../utils/aggregate.sh $(KIND)
	/usr/bin/time -f '{"compiler": "$(COMPILER)", "time":"%E"}' \
	  bash -c 'for f in $(PROBLEMS); do node $(SCRIPT) $$f || exit 1; done' 2>&1 1> /dev/null | \
	sh ../utils/format_metrics.sh exec | \
	sh ../utils/aggregate.sh $(KIND)
