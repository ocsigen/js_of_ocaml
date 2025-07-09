.PHONY: bench perform

export NAME=Others
export SUBNAME=bin_prot

SHELL=/usr/bin/env bash -o pipefail

bench:
	@date -u +"%FT%TZ - $(NAME)/$(SUBNAME): starting"
	mkdir -p ../../../../janestreet/bench/bin_prot/
	cp -f dune bench.ml ../../../../janestreet/bench/bin_prot/
	cd ../../../../janestreet/bench/bin_prot && dune build --root ../.. --profile release bench/bin_prot/bench.bc.js bench/bin_prot/bench.bc.wasm.js
	cp -rf ../../../../janestreet/_build/default/bench/bin_prot/bench.bc* .
	node bench.bc.js 400000
	$(MAKE) perform COMPILER=js_of_ocaml SCRIPT=bench.bc.js KIND=js
	$(MAKE) perform COMPILER=wasm_of_ocaml SCRIPT=bench.bc.wasm.js KIND=wasm
	@date -u +"%FT%TZ - $(NAME)/$(SUBNAME): done"

perform:
	env time -f '{"compiler": "$(COMPILER)", "time":"%E"}' node $(SCRIPT) 2>&1 1> /dev/null | \
	tee /dev/stderr | \
	sh ../../utils/format_metrics.sh exec | \
	sh ../../utils/aggregate.sh $(KIND)
