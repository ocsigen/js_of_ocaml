.PHONY: bench perform

export NAME=Others
export SUBNAME=bigarrays

SHELL=/bin/bash -o pipefail

bench:
	@date -u +"%FT%TZ - $(NAME)/$(SUBNAME): starting"
	ocamlc bench.ml -o bench
	$(MAKE) perform COMPILER=js_of_ocaml SCRIPT=bench.js KIND=js
	$(MAKE) perform COMPILER=wasm_of_ocaml SCRIPT=bench.wasm.js KIND=wasm
	@date -u +"%FT%TZ - $(NAME)/$(SUBNAME): done"

perform:
	$(COMPILER) --opt 2 --pretty bench -o $(SCRIPT)
	/usr/bin/time -f '{"compiler": "$(COMPILER)", "time":"%E"}' node $(SCRIPT) 2>&1 | \
	sh ../../utils/format_metrics.sh exec | \
	sh ../../utils/aggregate.sh $(KIND)
