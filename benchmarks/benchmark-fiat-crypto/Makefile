.PHONY: bench perform

export NAME=Fiat-crypto

bench:
	$(MAKE) perform COMPILER=js_of_ocaml EXTRA_ARGS="--enable effects"
	$(MAKE) perform COMPILER=wasm_of_ocaml EXTRA_ARGS=""

perform: bedrock2_fiat_crypto.byte
	time -f '{"compiler": "$(COMPILER)", "time":"%E", "mem":%R}' $(COMPILER) --opt 2 --source-map $(EXTRA_ARGS) $< 2>&1 | sh ../utils/format_metrics.sh

bedrock2_fiat_crypto.byte: bedrock2_fiat_crypto.ml
	ocamlfind ocamlc -w -20 -g -linkpkg -package js_of_ocaml -g $< -o $@

bedrock2_fiat_crypto.ml: bedrock2_fiat_crypto.ml.xz
	unxz --keep $<
