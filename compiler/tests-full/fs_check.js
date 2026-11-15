const files = {};
globalThis.jsoo_create_file = (name, content) => {
  files[name] = content;
};
require("./fs.output.js");
const content = files["/static/file3"];
if (content.length !== 128) throw new Error(`length ${content.length}`);
for (let i = 0; i < 128; i++)
  if (content.charCodeAt(i) !== 0x80 + i) throw new Error(`byte ${i}`);
