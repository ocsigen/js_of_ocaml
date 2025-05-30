.PHONY: bench perform

export NAME=Others
export SUBNAME=bin_prot

SHELL=/bin/bash -o pipefail

bench:
	@date -u +"%FT%TZ - $(NAME)/$(SUBNAME): starting"
	dune build --profile release --root .
	node _build/default/bench.bc.js 400000
	$(MAKE) perform COMPILER=js_of_ocaml SCRIPT=_build/default/bench.bc.js KIND=js
	$(MAKE) perform COMPILER=wasm_of_ocaml SCRIPT=_build/default/bench.bc.wasm.js KIND=wasm
	@date -u +"%FT%TZ - $(NAME)/$(SUBNAME): done"

perform:
	/usr/bin/time -f '{"compiler": "$(COMPILER)", "time":"%E"}' node $(SCRIPT) 2>&1 1> /dev/null | \
	tee /dev/stderr | \
	sh ../../utils/format_metrics.sh exec | \
	sh ../../utils/aggregate.sh $(KIND)
