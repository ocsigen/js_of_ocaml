// Checks that files embedded by build-fs are read back unchanged by a
// program compiled with wasm_of_ocaml (see fs_check.js). The program,
// given as argument, starts asynchronously.
const path = require("node:path");
// Loaded before the program: the files are queued
require("./fs.output.js");
require(path.resolve(process.argv[2]));
let tries = 0;
(function wait() {
  if (!globalThis.jsooTestReadFile) {
    if (++tries > 1000) throw new Error("the program did not start");
    return setTimeout(wait, 10);
  }
  // Loaded after the program: the files are created directly
  require("./fs_late.output.js");
  require("./fs_check.js");
})();
