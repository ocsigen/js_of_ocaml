
# Module `_.Value`

```ocaml
val unit : expression
```
```ocaml
val val_int : expression -> expression
```
```ocaml
val int_val : expression -> expression
```
```ocaml
val check_is_not_zero : expression -> expression
```
Returns an int32 value

```ocaml
val check_is_int : expression -> expression
```
Returns an int32 value

```ocaml
val not : expression -> expression
```
```ocaml
val lt : expression -> expression -> expression
```
```ocaml
val le : expression -> expression -> expression
```
```ocaml
val js_eqeqeq : negate:bool -> expression -> expression -> expression
```
```ocaml
val phys_eq : expression -> expression -> expression
```
```ocaml
val phys_neq : expression -> expression -> expression
```
```ocaml
val ult : expression -> expression -> expression
```
```ocaml
val is_int : expression -> expression
```
```ocaml
val int_add : expression -> expression -> expression
```
```ocaml
val int_sub : expression -> expression -> expression
```
```ocaml
val int_mul : expression -> expression -> expression
```
```ocaml
val int_div : expression -> expression -> expression
```
```ocaml
val int_mod : expression -> expression -> expression
```
```ocaml
val int_neg : expression -> expression
```
```ocaml
val int_or : expression -> expression -> expression
```
```ocaml
val int_and : expression -> expression -> expression
```
```ocaml
val int_xor : expression -> expression -> expression
```
```ocaml
val int_lsl : expression -> expression -> expression
```
```ocaml
val int_lsr : expression -> expression -> expression
```
```ocaml
val int_asr : expression -> expression -> expression
```
```ocaml
val block_type : Wasm_ast.value_type Code_generation.t
```
```ocaml
val dummy_block : expression
```
```ocaml
val as_block : expression -> expression
```