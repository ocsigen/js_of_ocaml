# Module `Mlvalue.Block`

```ocaml
val make : 
  tag:int ->
  desc:Code.Block_desc.t ->
  args:Javascript.element list ->
  Javascript.expression
```
Introcaml: with introspection enabled, `desc` is stored in the header word, above the tag.

```ocaml
val reserved_bits : int
```
The number of reserved header bits (Introcaml), which hold the descriptor of a block.

```ocaml
val header : 
  tag:Javascript.expression ->
  reserved:Javascript.expression ->
  Javascript.expression
```
The header word of a block with the given tag and reserved bits; the reserved bits are dropped when introspection is disabled.

```ocaml
val max_header : unit -> Javascript.expression
```
The largest header word of a block.

```ocaml
val tag : Javascript.expression -> Javascript.expression
```
The tag of a block, without the descriptor bits.

```ocaml
val set_tag : 
  Javascript.expression ->
  Javascript.expression ->
  Javascript.expression
```
`set_tag e tag` updates the tag of block `e` in place, keeping its descriptor. `e` may be evaluated more than once.

```ocaml
val reserved : Javascript.expression -> Javascript.expression
```
The reserved header bits of a block.

```ocaml
val set_reserved : 
  Javascript.expression ->
  Javascript.expression ->
  Javascript.expression
```
`set_reserved e bits` updates the reserved header bits of block `e` in place, keeping its tag. `e` may be evaluated more than once.

```ocaml
val field : Javascript.expression -> int -> Javascript.expression
```
