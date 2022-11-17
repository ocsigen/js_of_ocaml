# Benchmarks 

These benchmarks are used to generate charts such as the ones available at https://ocsigen.org/js_of_ocaml/dev/manual/performances.

## Quick start
```
# build/copy additionnal bytecode executable inside `./programs/`
# used when generating the size report.
make copy-extra-bc

# Tune run.config to fit your environment

# running `make` will
# build all benchmarks to bytecode, native and javascript
# compute sizes of artifacts
# run benchmarks and record timings
# generate a bunch pdf reports
make
```

## Contents

| Filename        | Description                                    |
| -----------     | --------------------------------------------   |
| sources/ml      | OCaml benchmarks                               |
| sources/js      | JavaScript benchmarks                          |
| run.exe         | Generate size and timing data                  |
| run.config      | Configure available js interpreter             |
| report.exe      | Generate reports using gnuplot                 |
| report-*.config | config read by report.exe to generate a report |
