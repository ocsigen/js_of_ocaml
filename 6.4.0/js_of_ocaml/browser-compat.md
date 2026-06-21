
# Targeting older browsers

Js\_of\_ocaml's generated code and runtime target **ECMAScript 2020 (ES2020)**. Out of the box, the output runs on Node.js 18+, QuickJS-NG, and any evergreen browser released since early 2020 (Chrome 80+, Firefox 74+, Safari 13\.4+, Edge 80+).

If you need to support an older engine, you can post-process the generated JavaScript with [Babel](https://babeljs.io) and ship polyfills with [core-js](https://github.com/zloirock/core-js). Js\_of\_ocaml has no built-in downleveling.


## Transpiling with Babel

Install Babel and core-js:

```
npm install --save-dev @babel/core @babel/cli @babel/preset-env
npm install core-js
```
Create a `babel.config.json` next to your build:

```json
{
  "presets": [
    ["@babel/preset-env", {
      "targets": "> 0.5%, last 2 versions, not dead",
      "useBuiltIns": "usage",
      "corejs": 3
    }]
  ]
}
```
Set `targets` to the [browserslist query](https://github.com/browserslist/browserslist) matching the engines you need to support.

Compile your program with js\_of\_ocaml, then run Babel on the output:

```
js_of_ocaml program.byte -o program.js
npx babel program.js -o program.es5.js
```
The resulting `program.es5.js` imports polyfills from `core-js` with `require("core-js/modules/...")` calls. Before shipping to a browser you need to bundle it so those imports are resolved and inlined.


## Bundling

Any JavaScript bundler will work; [esbuild](https://esbuild.github.io) is a small, zero-config option:

```
npm install --save-dev esbuild
npx esbuild program.es5.js --bundle --platform=browser \
  --external:node:* --target=es5 --outfile=program.bundle.js
```
The runtime contains a few `require("node:*")` calls (filesystem, tty, etc.) guarded by Node-detection branches that never run in a browser. `--external:node:*` tells esbuild to leave those imports in place instead of failing to resolve them at bundle time.

[`--target`](https://esbuild.github.io/api/#target) is needed even when Babel has lowered the input: esbuild's own IIFE wrapper and CommonJS helpers use arrow functions and other post-ES5 syntax, and `--target` is what tells it to lower those too. Set it to the lowest syntax level your browserslist query implies.

Ship `program.bundle.js` as a single script; no additional polyfills are required at load time.


## With dune

Assuming dune builds your program to `program.bc.js` (e.g. from an `(executable (modes js) ...)` stanza), wire the Babel and esbuild steps into the build with two rules:

```
(rule
 (targets program.es5.js)
 (deps program.bc.js babel.config.json)
 (action
  (run npx babel %{dep:program.bc.js} -o %{targets})))

(rule
 (targets program.bundle.js)
 (deps program.es5.js)
 (action
  (run npx esbuild %{dep:program.es5.js}
       --bundle --platform=browser --external:node:*
       --target=es5 --outfile=%{targets})))
```

## See also

- [Command line options](./options.md) — compiler flags
- [Installation](./install.md) — supported engines