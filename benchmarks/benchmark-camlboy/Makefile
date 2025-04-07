.PHONY: bench perform

export NAME=Camlboy

SHELL=/bin/bash -o pipefail

DIR=../../../CAMLBOY
SCRIPT=$(DIR)/_build/default/bin/web/bench_node.bc
ROM=$(DIR)/resource/games/tobu.gb

bench:
	$(MAKE) perform COMPILER=Js_of_ocaml SUFFIX=.js KIND=js
	$(MAKE) perform COMPILER=Wasm_of_ocaml SUFFIX=.wasm.js KIND=wasm

perform:
	node $(SCRIPT)$(SUFFIX) $(ROM) | \
	tee /dev/stderr | \
	ocaml output_results.ml $(COMPILER) | \
	sh ../utils/aggregate.sh $(KIND)
