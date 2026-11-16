// Checks that files embedded by build-fs are read back unchanged by a
// program. This script is concatenated (see dune) after fs.output.js,
// loaded before the program (files are queued until the runtime is
// initialised), fs_read.js, the program, and fs_late.output.js, loaded
// after it (files are created directly). No require: it also runs with
// QuickJS.
(() => {
  const read = globalThis.jsooTestReadFile;
  const check = (name, expected) => {
    const actual = read(name);
    if (actual !== expected)
      throw new Error(`${name}: got ${JSON.stringify(actual)}`);
  };
  let file3 = "";
  for (let i = 0; i < 128; i++) file3 += String.fromCharCode(0x80 + i);
  check("/static/file1", "This is file 1");
  check("/static/dir/file2", "This is file 2");
  check("/static/file3", file3);
  check("/static/late/file3", file3);
})();
