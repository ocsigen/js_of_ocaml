
# Module `Js_parser.MenhirInterpreter`

```ocaml
type token = token
```
```ocaml
type production
```
```ocaml
type 'a env
```
```ocaml
type !'a checkpoint = private 
  | InputNeeded of 'a env
  | Shifting of 'a env * 'a env * bool
  | AboutToReduce of 'a env * production
  | HandlingError of 'a env
  | Accepted of 'a
  | Rejected
```
```ocaml
val offer : 
  'a checkpoint ->
  (token
   * MenhirLib__IncrementalEngine.position
   * MenhirLib__IncrementalEngine.position) ->
  'a checkpoint
```
```ocaml
type strategy = [ 
  | `Legacy
  | `Simplified
 ]
```
```ocaml
val resume : ?strategy:strategy -> 'a checkpoint -> 'a checkpoint
```
```ocaml
type supplier =
  unit ->
  token
  * MenhirLib__IncrementalEngine.position
  * MenhirLib__IncrementalEngine.position
```
```ocaml
val lexer_lexbuf_to_supplier : 
  (Stdlib.Lexing.lexbuf -> token) ->
  Stdlib.Lexing.lexbuf ->
  supplier
```
```ocaml
val loop : ?strategy:strategy -> supplier -> 'a checkpoint -> 'a
```
```ocaml
val loop_handle : 
  ('a -> 'answer) ->
  ('a checkpoint -> 'answer) ->
  supplier ->
  'a checkpoint ->
  'answer
```
```ocaml
val loop_handle_undo : 
  ('a -> 'answer) ->
  ('a checkpoint -> 'a checkpoint -> 'answer) ->
  supplier ->
  'a checkpoint ->
  'answer
```
```ocaml
val shifts : 'a checkpoint -> 'a env option
```
```ocaml
val acceptable : 
  'a checkpoint ->
  token ->
  MenhirLib__IncrementalEngine.position ->
  bool
```
```ocaml
type 'a lr1state
```
```ocaml
val number : 'a lr1state -> int
```
```ocaml
val production_index : production -> int
```
```ocaml
val find_production : int -> production
```
```ocaml
type element = 
  | Element : 'a lr1state
    * 'a
    * MenhirLib__IncrementalEngine.position
    * MenhirLib__IncrementalEngine.position -> element
```
```ocaml
val top : 'a env -> element option
```
```ocaml
val pop_many : int -> 'a env -> 'a env option
```
```ocaml
val get : int -> 'a env -> element option
```
```ocaml
val current_state_number : 'a env -> int
```
```ocaml
val equal : 'a env -> 'a env -> bool
```
```ocaml
val positions : 
  'a env ->
  MenhirLib__IncrementalEngine.position * MenhirLib__IncrementalEngine.position
```
```ocaml
val env_has_default_reduction : 'a env -> bool
```
```ocaml
val state_has_default_reduction : 'a lr1state -> bool
```
```ocaml
val pop : 'a env -> 'a env option
```
```ocaml
val force_reduction : production -> 'a env -> 'a env
```
```ocaml
val input_needed : 'a env -> 'a checkpoint
```