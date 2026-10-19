.PHONY: bench perform

# herd7, the memory model simulator from herdtools7
# (https://github.com/herd/herdtools7), which also has a web version
# compiled with js_of_ocaml: CPU-bound enumeration of candidate executions.

export NAME=Herd

SHELL=/bin/bash -o pipefail

VERSION=7.58
DIR=herdtools7
BC=$(DIR)/_build/default/herd/herd.bc
ZARITH=$(shell opam var zarith_stubs_js:lib)/runtime.js
ZARITH_WAT=$(shell opam var zarith_stubs_js:lib)/runtime.wat

bench: $(BC)
	@date -u +"%FT%TZ - $(NAME): starting"
	$(MAKE) perform COMPILER=js_of_ocaml SCRIPT=herd.js KIND=js STUBS="$(ZARITH)"
	$(MAKE) perform COMPILER=wasm_of_ocaml SCRIPT=herd.wasm.js KIND=wasm STUBS="$(ZARITH) $(ZARITH_WAT)"
	@date -u +"%FT%TZ - $(NAME): done"

$(BC):
	git clone --depth 1 -b $(VERSION) https://github.com/herd/herdtools7 $(DIR)
	cd $(DIR) && sh ./version-gen.sh /usr/local
	sed -i 's/^ (name herd)$$/ (name herd)\n (modes byte exe)/' $(DIR)/herd/dune
	dune build --root $(DIR) --profile release herd/herd.bc

# The 70 AArch64 litmus tests of the catalogue
CATALOGUE=$(DIR)/catalogue/aarch64
ARGS=-set-libdir $(DIR)/herd/libdir -I $(CATALOGUE)/cats $(CATALOGUE)/tests/*.litmus

perform:
	/usr/bin/time -f "%E %M" $(COMPILER) --debug times $(STUBS) $(BC) -o $(SCRIPT) 2>&1 | \
	ocaml -I +str str.cma ../utils/compilation_metrics.ml $(COMPILER) $(NAME) $(SCRIPT) | \
	sh ../utils/aggregate.sh $(KIND)
	/usr/bin/time -f '{"compiler": "$(COMPILER)", "time":"%E"}' node $(SCRIPT) $(ARGS) 2>&1 1> /dev/null | \
	sh ../utils/format_metrics.sh exec | \
	sh ../utils/aggregate.sh $(KIND)
