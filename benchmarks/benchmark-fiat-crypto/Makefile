.PHONY: bench perform

export NAME=Fiat-Crypto

SHELL=/bin/bash -o pipefail

bench:
	$(MAKE) perform COMPILER=js_of_ocaml EXTRA_ARGS="--enable effects"
	$(MAKE) perform COMPILER=wasm_of_ocaml EXTRA_ARGS=""

perform: bedrock2_fiat_crypto.byte
	/usr/bin/time -f "%E %R" $(COMPILER) --debug times --source-map $(EXTRA_ARGS) $< 2>&1 | \
	ocaml -I +str str.cma ../utils/compilation_metrics.ml $(COMPILER) $(NAME)

bedrock2_fiat_crypto.byte: bedrock2_fiat_crypto.ml
	ocamlfind ocamlc -w -20 -g -linkpkg -package js_of_ocaml -g $< -o $@

bedrock2_fiat_crypto.ml: bedrock2_fiat_crypto.ml.xz
	unxz --keep $<
