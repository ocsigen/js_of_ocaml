const fs = require('fs/promises');
const path = require('path');

async function main() {
    const runtimePath =
          path.resolve(path.dirname(process.argv[1]), 'runtime.wasm');
    const runtime = fs.readFile(runtimePath);
    const code = fs.readFile(process.argv[2]);
    let math =
        {cos:Math.cos, sin:Math.sin, asin:Math.asin, atan2:Math.atan2,
         pow:Math.pow, fmod:(x, y) => x%y}
    const runtimeModule =
          await WebAssembly.instantiate(await runtime, {Math:math});
    const wasmModule =
          await WebAssembly.instantiate(await code,
                                        {env:runtimeModule.instance.exports,
                                         Math:math})
    try {
      wasmModule.instance.exports._initialize()
    } catch (e) {
        if (e instanceof WebAssembly.Exception &&
            e.is(runtimeModule.instance.exports.ocaml_exit))
            process.exit(e.getArg(runtimeModule.instance.exports.ocaml_exit, 0));
        throw e;
    }
}

main ()
