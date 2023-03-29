open! Stdlib
module W = Wa_ast
open Wa_code_generation

type expression = Wa_ast.expression Wa_code_generation.t

module Value = struct
  let value : W.value_type = I32

  let unit = Arith.const 1l

  let val_int i = Arith.((i lsl const 1l) + const 1l)

  let int_val i = Arith.(i asr const 1l)

  let check_is_not_zero i = Arith.(i <> const 1l)

  let check_is_int i = Arith.(i land const 1l)

  let not b = Arith.(const 4l - b)

  let lt i i' = val_int Arith.(i < i')

  let le i i' = val_int Arith.(i <= i')

  let eq i i' = val_int Arith.(i = i')

  let neq i i' = val_int Arith.(i <> i')

  let ult i i' = val_int Arith.(ult i i')

  let is_int i = val_int Arith.(i land const 1l)

  let int_add i i' = Arith.(i + i' - const 1l)

  let int_sub i i' = Arith.(i - i' + const 1l)

  let int_mul i i' = val_int Arith.(int_val i * int_val i')

  let int_neg i = Arith.(const 2l - i)

  let int_or i i' = Arith.(i lor i')

  let int_and i i' = Arith.(i land i')

  let int_xor i i' = Arith.(i lxor i' lor const 1l)

  let int_lsl i i' = Arith.(((i - const 1l) lsl int_val i') + const 1l)

  let int_lsr i i' = Arith.((i lsr int_val i') lor const 1l)

  let int_asr i i' = Arith.((i asr int_val i') lor const 1l)
end
