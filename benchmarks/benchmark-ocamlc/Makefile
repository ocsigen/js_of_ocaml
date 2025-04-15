.PHONY: bench perform

export NAME=Ocamlc

SHELL=/bin/bash -o pipefail

bench:
	@date -u +"%FT%TZ - $(NAME): starting"
	cp -r ../sources/ml .
	$(MAKE) perform COMPILER=js_of_ocaml SCRIPT=ocamlc.js KIND=js
	$(MAKE) perform COMPILER=wasm_of_ocaml SCRIPT=ocamlc.wasm.js KIND=wasm
	@date -u +"%FT%TZ - $(NAME): done"

ARGS=ml/*.ml ml/*.ml ml/*.ml ml/*.ml ml/*.ml ml/*.ml ml/*.ml ml/*.ml

perform:
	/usr/bin/time -f "%E %R" $(COMPILER) --debug times --opt 2 --pretty `which ocamlc.byte` -o $(SCRIPT) 2>&1 | \
	ocaml -I +str str.cma ../utils/compilation_metrics.ml $(COMPILER) $(NAME) $(SCRIPT) | \
	sh ../utils/aggregate.sh $(KIND)
	/usr/bin/time -f '{"compiler": "$(COMPILER)", "time":"%E"}' node $(SCRIPT) -c $(ARGS) 2>&1 | \
	sh ../utils/format_metrics.sh exec | \
	sh ../utils/aggregate.sh $(KIND)
