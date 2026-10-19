.PHONY: bench perform

# query-json, a jq implementation (https://github.com/davesnx/query-json):
# JSON lexing, parsing and printing. It relies on effect handlers, so the
# JavaScript version is compiled with --effects cps, as upstream does.

export NAME=Query-json

SHELL=/bin/bash -o pipefail

VERSION=0.6.1
DIR=query-json
BC=$(DIR)/_build/default/bin/bin.bc
DATA=$(DIR)/benchmarks
ZARITH=$(shell ocamlfind query zarith_stubs_js)/runtime.js

bench: $(BC)
	@date -u +"%FT%TZ - $(NAME): starting"
	$(MAKE) perform COMPILER=js_of_ocaml SCRIPT=query-json.js KIND=js FLAGS="--effects cps"
	$(MAKE) perform COMPILER=wasm_of_ocaml SCRIPT=query-json.wasm.js KIND=wasm
	@date -u +"%FT%TZ - $(NAME): done"

$(BC):
	git clone --depth 1 -b $(VERSION) https://github.com/davesnx/query-json $(DIR)
	sed -i 's/(modes native)/(modes byte exe)/' $(DIR)/bin/dune
	dune build --root $(DIR) --profile release bin/bin.bc

perform:
	/usr/bin/time -f "%E %M" $(COMPILER) --debug times $(FLAGS) $(ZARITH) $(BC) -o $(SCRIPT) 2>&1 | \
	ocaml -I +str str.cma ../utils/compilation_metrics.ml $(COMPILER) $(NAME) $(SCRIPT) | \
	sh ../utils/aggregate.sh $(KIND)
	/usr/bin/time -f '{"compiler": "$(COMPILER)", "time":"%E"}' bash run.sh $(SCRIPT) $(DATA) 2>&1 1> /dev/null | \
	sh ../utils/format_metrics.sh exec | \
	sh ../utils/aggregate.sh $(KIND)
