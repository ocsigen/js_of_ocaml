Tests of the Wasm linker.

  $ asm () { wasm-as --enable-gc --enable-reference-types --enable-exception-handling --enable-simd --enable-threads --enable-extended-const "$1.wat" -o "$1.wasm"; }

Active segments without an explicit memory or table index refer to memory 0
and table 0 of their module, which are not memory 0 and table 0 of the linked
module when several modules define a memory or a table.

  $ cat > memA.wat <<EOF
  > (module
  >   (memory 1) (data (i32.const 0) "A")
  >   (table 2 funcref) (elem (i32.const 0) \$f)
  >   (type \$t (func (result i32)))
  >   (func \$f (result i32) (i32.const 1))
  >   (func (export "loadA") (result i32) (i32.load8_u (i32.const 0)))
  >   (func (export "callA") (result i32) (call_indirect (type \$t) (i32.const 0))))
  > EOF
  $ cat > memB.wat <<EOF
  > (module
  >   (memory 1) (data (i32.const 0) "B")
  >   (table 2 funcref) (elem (i32.const 1) \$f)
  >   (type \$t (func (result i32)))
  >   (func \$f (result i32) (i32.const 2))
  >   (func (export "loadB") (result i32) (i32.load8_u (i32.const 0)))
  >   (func (export "callB") (result i32) (call_indirect (type \$t) (i32.const 1))))
  > EOF
  $ asm memA; asm memB
  $ ./link_driver.exe mem.wasm a:memA.wasm b:memB.wasm
  $ node run.cjs mem.wasm loadA loadB callA callB
  loadA: 65
  loadB: 66
  callA: 1
  callB: 2

Same thing with an element segment with an explicit table index.

  $ cat > tab.wat <<EOF
  > (module
  >   (table \$t0 1 funcref)
  >   (table \$t1 1 funcref)
  >   (type \$t (func (result i32)))
  >   (func \$f (result i32) (i32.const 3))
  >   (elem (table \$t1) (i32.const 0) func \$f)
  >   (func (export "call") (result i32) (call_indirect \$t1 (type \$t) (i32.const 0))))
  > EOF
  $ asm tab
  $ ./link_driver.exe tab2.wasm a:memA.wasm b:tab.wasm
  $ node run.cjs tab2.wasm callA call
  callA: 1
  call: 3

Legacy exception handling instructions.

  $ cat > legacy.wat <<EOF
  > (module
  >   (tag \$e (param i32))
  >   (tag \$e2)
  >   (func (export "catchAll") (result i32)
  >     (try (result i32)
  >       (do (throw \$e2))
  >       (catch \$e)
  >       (catch_all (i32.const 2))))
  >   (func (export "delegate") (result i32)
  >     (try \$l (result i32)
  >       (do
  >         (try (result i32)
  >           (do (throw \$e (i32.const 3)))
  >           (delegate \$l)))
  >       (catch \$e))))
  > EOF
  $ asm legacy
  $ ./link_driver.exe legacy2.wasm a:memA.wasm b:legacy.wasm
  $ node run.cjs legacy2.wasm catchAll delegate
  catchAll: 2
  delegate: 3

SIMD instructions. The global index of [global.get] after [v128.const] is
changed by linking.

  $ cat > simdA.wat <<EOF
  > (module (global (export "a") i32 (i32.const 100)))
  > EOF
  $ cat > simdB.wat <<EOF
  > (module
  >   (global \$x i32 (i32.const 10))
  >   (global \$y i32 (i32.const 20))
  >   (func (export "f") (result i32)
  >     (drop (v128.const i32x4 0 0 0 0))
  >     (drop (i8x16.extract_lane_s 3 (v128.const i32x4 0 0 0 0)))
  >     (global.get \$y)))
  > EOF
  $ asm simdA; asm simdB
  $ ./link_driver.exe simd.wasm a:simdA.wasm b:simdB.wasm
  $ node run.cjs simd.wasm f
  f: 20

Atomic fence.

  $ cat > fence.wat <<EOF
  > (module (func (export "f") (result i32) (atomic.fence) (i32.const 0)))
  > EOF
  $ asm fence
  $ ./link_driver.exe fence2.wasm a:fence.wasm
  $ node run.cjs fence2.wasm f
  f: 0

A table initializer can refer to an imported global.

This module is written in binary, since older versions of wasm-as do not
support table initializers:
(module (import "env" "g" (global $g funcref)) (table 1 funcref (global.get $g)))

  $ printf '\000asm\001\000\000\000\002\012\001\003env\001g\003\160\000\004\011\001\100\000\160\000\001\043\000\013' > tinit.wasm
  $ ./link_driver.exe tinit2.wasm a:tinit.wasm

But not to a global defined in the linked module, since globals are defined
after tables.

  $ cat > tinitA.wat <<EOF
  > (module (global (export "g") funcref (ref.null func)))
  > EOF
  $ asm tinitA
  $ ./link_driver.exe tinit3.wasm env:tinitA.wasm a:tinit.wasm 2>&1 | grep -o "a table initializer refers to the global import env / g"
  a table initializer refers to the global import env / g

But it can refer to a global imported from a module which re-exports one of
its own imports, since this import is still an import of the linked module.

  $ cat > reexp.wat <<EOF
  > (module (import "x" "g" (global \$g funcref)) (export "g" (global \$g)))
  > EOF
  $ asm reexp
  $ ./link_driver.exe tinit4.wasm env:reexp.wasm a:tinit.wasm
  $ node run.cjs tinit4.wasm
  import global x g
  global g

Likewise, a global can be imported from a later module when it is a
re-exported import.

  $ cat > globA.wat <<EOF
  > (module
  >   (import "b" "g" (global \$g i32))
  >   (func (export "f") (result i32) (global.get \$g)))
  > EOF
  $ cat > globB.wat <<EOF
  > (module (import "x" "g" (global \$g i32)) (export "g" (global \$g)))
  > EOF
  $ asm globA; asm globB
  $ ./link_driver.exe glob.wasm a:globA.wasm b:globB.wasm
  $ node run.cjs glob.wasm
  import global x g
  function f
  global g

Exception references.

  $ cat > exntab.wat <<EOF
  > (module
  >   (table 1 exnref)
  >   (func (export "f") (result i32) (ref.is_null (table.get 0 (i32.const 0)))))
  > EOF
  $ asm exntab
  $ ./link_driver.exe exntab2.wasm a:exntab.wasm
  $ node run.cjs exntab2.wasm f
  f: 1

A function can be imported with a supertype of its type, also when the
supertype is in the same recursion group.

  $ cat > subA.wat <<EOF
  > (module
  >   (rec (type \$super (sub (func (result i32)))) (type \$sub (sub \$super (func (result i32)))))
  >   (func (export "f") (type \$sub) (i32.const 5)))
  > EOF
  $ cat > subB.wat <<EOF
  > (module
  >   (rec (type \$super (sub (func (result i32)))) (type \$sub (sub \$super (func (result i32)))))
  >   (import "a" "f" (func \$g (type \$super)))
  >   (func (export "h") (result i32) (call \$g)))
  > EOF
  $ asm subA; asm subB
  $ ./link_driver.exe sub.wasm a:subA.wasm b:subB.wasm
  $ node run.cjs sub.wasm h
  h: 5

The initializer of a global can refer to a global defined in a later module.

  $ cat > gA.wat <<EOF
  > (module
  >   (import "b" "h" (global \$h i32))
  >   (global \$g i32 (global.get \$h))
  >   (global \$k (export "k") i32 (i32.const 7))
  >   (func (export "main") (result i32) (i32.add (global.get \$g) (global.get \$k))))
  > EOF
  $ cat > gB.wat <<EOF
  > (module
  >   (import "a" "k" (global \$k i32))
  >   (global \$h (export "h") i32 (i32.add (global.get \$k) (i32.const 5))))
  > EOF
  $ asm gA; asm gB
  $ ./link_driver.exe g.wasm a:gA.wasm b:gB.wasm
  $ node run.cjs g.wasm main
  main: 19

Exports are kept in order.

  $ cat > exp.wat <<EOF
  > (module (func (export "f1")) (func (export "f2")) (func (export "f3")))
  > EOF
  $ asm exp
  $ ./link_driver.exe exp2.wasm a:exp.wasm
  $ node run.cjs exp2.wasm
  function f1
  function f2
  function f3

Continuation type indices are signed LEB128 numbers (s33): index 64 is
encoded as 0xC0 0x00. This module defines 65 function types and a
continuation type of type 64.

  $ { printf '\000asm\001\000\000\000\001\307\001\102'
  >   for i in $(seq 65); do printf '\140\000\000'; done
  >   printf '\135\300\000'; } > cont.wasm
  $ ./link_driver.exe cont2.wasm a:cont.wasm

Dead code elimination. A function referenced by [ref.func] must remain
declared when the export and the global which declared it are removed.

  $ cat > decl.wat <<EOF
  > (module
  >   (func \$h (export "h") (result i32) (i32.const 6))
  >   (global \$g funcref (ref.func \$h))
  >   (func (export "main") (result i32) (call_ref \$t (ref.func \$h)))
  >   (type \$t (func (result i32))))
  > EOF
  $ cat > deps.json <<EOF
  > [{"name": "root", "reaches": ["main"], "root": true},
  >  {"name": "main", "export": "main"}]
  > EOF
  $ asm decl
  $ ./link_driver.exe -deps deps.json decl2.wasm a:decl.wasm
  $ node run.cjs decl2.wasm
  function main
  $ node run.cjs decl2.wasm main
  main: 6

Passive segments which are not used only keep the live functions they
mention, as declarations. Segments of expressions become segments of
function indices, since their type may be removed.

  $ cat > elem.wat <<EOF
  > (module
  >   (type \$t (func (result i32)))
  >   (type \$u (func (result i64)))
  >   (func \$h (result i32) (i32.const 6))
  >   (func \$dead (result i32) (i32.const 7))
  >   (func \$dead2 (result i64) (i64.const 8))
  >   (elem (ref null \$t) (item (ref.func \$h)) (item (ref.func \$dead)) (item (ref.null \$t)))
  >   (elem (ref null \$u) (item (ref.func \$dead2)))
  >   (func (export "main") (result i32) (call_ref \$t (ref.func \$h))))
  > EOF
  $ asm elem
  $ ./link_driver.exe -deps deps.json elem2.wasm a:elem.wasm
  $ wasm-dis elem2.wasm | grep -c "^ (func "
  2
  $ wasm-dis elem2.wasm | grep "(elem"
   (elem $0 func $0)
   (elem $1 func)
  $ node run.cjs elem2.wasm main
  main: 6

A table initializer declares the functions it refers to: no declarative
segment is added for them. This module, with a table initialized with
[ref.func $h] and a function returning [call_ref (ref.func $h)], cannot
be written in the text format accepted by our version of Binaryen. The
linked module has the same size, since nothing is added.

  $ printf '\000asm\001\000\000\000\001\005\001\140\000\001\177\003\003\002\000\000\004\011\001\100\000\160\000\001\322\000\013\007\010\001\004main\000\001\012\015\002\004\000\101\006\013\006\000\322\000\024\000\013' > tab.wasm
  $ ./link_driver.exe -deps deps.json tab2.wasm a:tab.wasm
  $ test $(wc -c < tab.wasm) -eq $(wc -c < tab2.wasm) && echo same size
  same size
  $ node run.cjs tab2.wasm main
  main: 6

Exports not selected by the filter are removed even if the dependency graph
reaches them.

  $ cat > deps2.json <<EOF
  > [{"name": "root", "reaches": ["main", "h"], "root": true},
  >  {"name": "main", "export": "main"},
  >  {"name": "h", "export": "h"}]
  > EOF
  $ ./link_driver.exe -deps deps2.json -filter main decl3.wasm a:decl.wasm
  $ node run.cjs decl3.wasm
  function main

Without dead code elimination, a function only declared by an export
which is removed gets declared in a declarative segment. In this module,
[main] returns [call_ref (ref.func $h)] and [$h] is exported; there is
no element segment, which Binaryen would add.

  $ printf '\000asm\001\000\000\000\001\005\001`\000\001\177\003\003\002\000\000\007\014\002\001h\000\000\004main\000\001\012\015\002\004\000A\006\013\006\000\322\000\024\000\013' > nodecl.wasm
  $ ./link_driver.exe -filter main nodecl2.wasm a:nodecl.wasm
  $ node run.cjs nodecl2.wasm
  function main
  $ node run.cjs nodecl2.wasm main
  main: 6

Global initializers reading each other in a cycle cannot be ordered:

  $ cat > cyc1.wat <<EOF
  > (module
  >   (import "b" "gb" (global \$gb i32))
  >   (global (export "ga") i32 (global.get \$gb)))
  > EOF
  $ cat > cyc2.wat <<EOF
  > (module
  >   (import "a" "ga" (global \$ga i32))
  >   (global (export "gb") i32 (global.get \$ga)))
  > EOF
  $ asm cyc1; asm cyc2
  $ ./link_driver.exe cyc.wasm a:cyc1.wasm b:cyc2.wasm 2>&1 | grep -o "The initializers of some globals of cyc1.wasm, cyc2.wasm read each other in a cycle"
  The initializers of some globals of cyc1.wasm, cyc2.wasm read each other in a cycle

Imports are checked against the exports they resolve to, also when dead
code elimination removes them:

  $ cat > badimp.wat <<EOF
  > (module
  >   (import "s" "f" (func (param i64) (result f32)))
  >   (import "s" "t" (tag (param f64)))
  >   (func (export "main") (result i32) (i32.const 0)))
  > EOF
  $ cat > badexp.wat <<EOF
  > (module
  >   (tag (export "t") (param i32))
  >   (func (export "f") (result i32) (i32.const 1)))
  > EOF
  $ asm badimp; asm badexp
  $ ./link_driver.exe -deps deps.json bad.wasm m:badimp.wasm s:badexp.wasm 2>&1 | grep -o "of an incompatible type"
  of an incompatible type

Tables and memories are always kept, so their import nodes are reached:

  $ cat > memimp.wat <<EOF
  > (module
  >   (import "env" "mem" (memory 1))
  >   (func (export "main") (result i32) (i32.load (i32.const 0)))
  >   (func (export "cb_mem") (result i32) (i32.const 1))
  >   (func (export "cb_unused") (result i32) (i32.const 3)))
  > EOF
  $ cat > memdeps.json <<EOF
  > [{"name": "root", "root": true, "reaches": ["main"]},
  >  {"name": "main", "export": "main"},
  >  {"name": "cb_mem", "export": "cb_mem"},
  >  {"name": "cb_unused", "export": "cb_unused"},
  >  {"name": "mem", "import": ["env", "mem"], "reaches": ["cb_mem"]},
  >  {"name": "g", "import": ["env", "g"], "reaches": ["cb_unused"]}]
  > EOF
  $ asm memimp
  $ ./link_driver.exe -deps memdeps.json memimp2.wasm a:memimp.wasm
  $ wasm-dis memimp2.wasm | grep export
   (export "main" (func $0))
   (export "cb_mem" (func $1))
