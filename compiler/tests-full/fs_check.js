// Checks that files embedded by build-fs are read back unchanged by a
// program, whether build-fs's output is loaded before the program (files
// are queued until the runtime is initialised) or after it (files are
// created directly).
const assert = require("node:assert");

const file3 = String.fromCharCode(
  ...Array.from({ length: 128 }, (_, i) => 0x80 + i),
);

require("./fs.output.js");
require("./fs_read.js");
const read = globalThis.jsooTestReadFile;
assert.strictEqual(read("/static/file1"), "This is file 1");
assert.strictEqual(read("/static/dir/file2"), "This is file 2");
assert.strictEqual(read("/static/file3"), file3);

require("./fs_late.output.js");
assert.strictEqual(read("/static/late/file3"), file3);
