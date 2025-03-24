.PHONY: bench perform

export NAME=Ocamlc

bench:
	cp -r ../sources/ml .
	$(MAKE) perform COMPILER=js_of_ocaml SCRIPT=ocamlc.js
	$(MAKE) perform COMPILER=wasm_of_ocaml SCRIPT=ocamlc.wasm.js

ARGS=ml/*.ml ml/*.ml ml/*.ml ml/*.ml ml/*.ml ml/*.ml ml/*.ml ml/*.ml

perform:
	time -f "%E %R" $(COMPILER) --debug times --opt 2 --pretty `which ocamlc.byte` -o $(SCRIPT) 2>&1 | ocaml -I +str str.cma ../utils/compilation_metrics.ml $(COMPILER) $(NAME)
	time -f '{"compiler": "$(COMPILER)", "time":"%E"}' node $(SCRIPT) -c $(ARGS) 2>&1 | sh ../utils/format_metrics.sh exec
