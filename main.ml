(*XXX
Compile to CPS + trampoline
Store exception continuation in a global datastructure


Semantics of loops???
   (of functions defined in a loop)
  ==> the loops should have their own contexts
====> nest blocks???


Generate actual code (!)

Approximation of variable contents:
  - function with arity
  - single variable
  - block with known values
  - array or fixed-size block + do we need the tag
    (block of this size (any size) with this tag (any tag))

Can we use the debugger information to generate specialized code?
(Use objects rather than arrays for tuples, ...)

(start with void; make it grow)
==> use this to get rid of global blocks
==> should be useful for optimizing function calls

Optimizations:
==> unused variables (dead call elimination)
==> coloring (eliminate (most) phi functions)
==> global access shortcut (shape propagation)
==> variables used only once (to build big expressions)
==> Shape analyze?  ==> specialized representations

==> call shape analyse??? is that possible?
    for avoiding cps transformation + trampoline when possible

- Build the control graph
- Compile each block
  ==> use fresh variables at each start of a block
      + add mapping from old to new variables


- Analyse du flot de contrôle...
  ==> or use CPS / extra functions???
- Implementation de l'appel de fonction???

NOTE: cheap trampoline:
   return fun () {...}
*)

(****)

open Util

open Instr

let blocks = ref IntSet.empty
let stops = ref IntSet.empty
let entries = ref IntSet.empty
let jumps = ref []

let add_start info pc =
(*  Format.eprintf "==> %d@." pc;*)
  blocks := IntSet.add pc !blocks

let add_stop info pc =
(*  Format.eprintf "==| %d@." pc;*)
  stops := IntSet.add pc !stops

let add_jump info pc pc' =
(*  Format.eprintf "==> %d --> %d@." pc pc';*)
  blocks := IntSet.add pc' !blocks;
  jumps := (pc, pc') :: !jumps

let add_entry info pc =
  add_start info pc;
  entries := IntSet.add pc !entries

let rec scan info code pc len =
  if pc < len then begin
    match (get_instr code pc).kind with
      KNullary ->
        scan info code (pc + 1) len
    | KUnary ->
        scan info code (pc + 2) len
    | KBinary ->
        scan info code (pc + 3) len
    | KJump ->
        let offset = gets code (pc + 1) in
        add_jump info pc (pc + offset + 1);
        add_stop info pc;
        scan info code (pc + 2) len
    | KCond_jump ->
        let offset = gets code (pc + 1) in
        add_jump info pc (pc + offset + 1);
        add_start info (pc + 2);
        scan info code (pc + 2) len
    | KCmp_jump ->
        let offset = gets code (pc + 2) in
        add_jump info pc (pc + offset + 2);
        add_start info (pc + 3);
        scan info code (pc + 3) len
    | KSwitch ->
        let sz = getu code (pc + 1) in
        for i = 0 to sz land 0xffff + sz lsr 16 - 1 do
          let offset = gets code (pc + 2 + i) in
          add_jump info pc (pc + offset + 2)
        done;
        add_stop info pc;
        scan info code (pc + 2 + sz land 0xffff + sz lsr 16) len
    | KClosurerec ->
        let nfuncs = getu code (pc + 1) in
        for i = 0 to nfuncs - 1 do
          let addr = pc + gets code (pc + 3 + i) + 3 in
          add_entry info addr
        done;
        scan info code (pc + nfuncs + 3) len
    | KClosure ->
        let addr = pc + gets code (pc + 2) + 2 in
        add_entry info addr;
        scan info code (pc + 3) len
    | KStop n ->
        add_stop info pc;
        scan info code (pc + n + 1) len
  end

let rec find_block pc =
  if IntSet.mem pc !blocks then pc else find_block (pc - 1)

let rec next_block len pc =
  let pc = pc + 1 in
  if pc = len || IntSet.mem pc !blocks then pc else next_block len pc

let map_of_set f s =
  IntSet.fold (fun pc m -> IntMap.add pc (f pc) m) s IntMap.empty

let analyse_blocks code =
  let len = String.length code  / 4 in
  add_entry () 0;
  scan () code 0 len;
  let has_stop =
    IntSet.fold (fun pc hs -> IntSet.add (find_block pc) hs)
      !stops IntSet.empty
  in
  IntSet.iter
    (fun pc ->
       if not (IntSet.mem pc has_stop) then add_jump () pc (next_block len pc))
    !blocks;
  let cont = map_of_set (fun _ -> IntSet.empty) !blocks in
  let cont =
    List.fold_left
      (fun cont (pc, pc') ->
         let pc = find_block pc in
         IntMap.add pc (IntSet.add pc' (IntMap.find pc cont)) cont)
      cont !jumps
  in
  cont

(****)

open Code

type globals =
  { vars : Var.t option array;
    is_const : bool array;
    constants : Obj.t array;
    primitives : string array }

module State = struct

  type elt = Var of Var.t (*| Addr of int*) | Dummy

  let elt_to_var e = match e with Var x -> x | _ -> assert false
  let opt_elt_to_var e = match e with Var x -> Some x | _ -> None

  let print_elt f v =
    match v with
    | Var x   -> Format.fprintf f "%a" Var.print x
(*    | Addr x  -> Format.fprintf f "[%d]" x*)
    | Dummy   -> Format.fprintf f "???"

  type t =
    { accu : elt; stack : elt list; env : elt array; env_offset : int;
      var_stream : Var.stream; globals : globals }

  let fresh_var state =
    let (x, stream) = Var.next state.var_stream in
    (x, {state with var_stream = stream; accu = Var x})

  let globals st = st.globals

  let rec list_start n l =
    if n = 0 then [] else
    match l with
      []     -> assert false
    | v :: r -> v :: list_start (n - 1) r

  let rec st_pop n st =
    if n = 0 then
      st
    else
      match st with
        []     -> assert false
      | v :: r -> st_pop (n - 1) r

  let push st = {st with stack = st.accu :: st.stack}

  let pop n st = {st with stack = st_pop n st.stack}

  let acc n st = {st with accu = List.nth st.stack n}

  let env_acc n st = {st with accu = st.env.(st.env_offset + n)}

  let has_accu st = st.accu <> Dummy

  let accu st = elt_to_var st.accu

  let opt_accu st = opt_elt_to_var st.accu

  let set_accu st x = {st with accu = Var x}

  let clear_accu st = {st with accu = Dummy}

  let peek n st = elt_to_var (List.nth st.stack n)

  let grab n st = (List.map elt_to_var (list_start n st.stack), pop n st)

  let start_function state env offset =
    {state with accu = Dummy; stack = []; env = env; env_offset = offset}

  let start_block state =
    match state.accu with
      Dummy -> state
    | Var _ -> snd (fresh_var state)

  let initial g =
    { accu = Dummy; stack = []; env = [||]; env_offset = 0;
      var_stream = Var.make_stream (); globals = g }

  let rec print_stack f l =
    match l with
      [] -> ()
    | v :: r -> Format.fprintf f "%a %a" print_elt v print_stack r

  let print_env f e =
    Array.iteri
      (fun i v ->
         if i > 0 then Format.fprintf f " "; Format.fprintf f "%a" print_elt v) e

  let print st =
    Format.eprintf "{ %a | %a | (%d) %a }@."
      print_elt st.accu print_stack st.stack st.env_offset print_env st.env

end

let primitive_name state i =
  let g = State.globals state in
  assert (i >= 0 && i <= Array.length g.primitives);
  g.primitives.(i)

let compiled_block = ref IntMap.empty

let debug = false

let rec compile_block code orig pc state =
  if not (IntMap.mem pc !compiled_block) then begin
    let len = String.length code  / 4 in
    let limit = next_block len pc in
    if debug then Format.eprintf "Compiling from %d to %d@." pc (limit - 1);
    let state = State.start_block state in
    let (instr, last, state') = compile code limit pc state [] in
    compiled_block :=
      IntMap.add pc (state, List.rev instr, last) !compiled_block;
    begin match last with
      Branch (pc', _) ->
        compile_block code pc pc' state'
    | Cond (_, _, (pc1, _), (pc2, _)) ->
        compile_block code pc pc1 state';
        compile_block code pc pc2 state'
    | Switch (_, l1, l2) ->
        Array.iter (fun (pc', _) -> compile_block code pc pc' state') l1;
        Array.iter (fun (pc', _) -> compile_block code pc pc' state') l2
    | Pushtrap _ | Raise _ | Return _ | Stop ->
        ()
    end
  end

and compile code limit pc state instrs =
  if debug then State.print state;
  if pc = limit then
    (instrs, Branch (pc, State.opt_accu state), state)
  else begin
  if debug then Format.eprintf "%4d " pc;
  let instr =
    try
      get_instr code pc
    with Bad_instruction op ->
      if debug then Format.eprintf "%08x@." op;
      assert false
  in
  if debug then Format.eprintf "%08x %s@." instr.opcode instr.name;
  match instr.code with
  | ACC0 ->
      compile code limit (pc + 1) (State.acc 0 state) instrs
  | ACC1 ->
      compile code limit (pc + 1) (State.acc 1 state) instrs
  | ACC2 ->
      compile code limit (pc + 1) (State.acc 2 state) instrs
  | ACC3 ->
      compile code limit (pc + 1) (State.acc 3 state) instrs
  | ACC4 ->
      compile code limit (pc + 1) (State.acc 4 state) instrs
  | ACC5 ->
      compile code limit (pc + 1) (State.acc 5 state) instrs
  | ACC6 ->
      compile code limit (pc + 1) (State.acc 6 state) instrs
  | ACC7 ->
      compile code limit (pc + 1) (State.acc 7 state) instrs
  | ACC ->
      let n = getu code (pc + 1) in
      compile code limit (pc + 2) (State.acc n state) instrs
  | PUSH ->
      compile code limit (pc + 1) (State.push state) instrs
  | PUSHACC0 ->
      compile code limit (pc + 1) (State.acc 0 (State.push state)) instrs
  | PUSHACC1 ->
      compile code limit (pc + 1) (State.acc 1 (State.push state)) instrs
  | PUSHACC2 ->
      compile code limit (pc + 1) (State.acc 2 (State.push state)) instrs
  | PUSHACC3 ->
      compile code limit (pc + 1) (State.acc 3 (State.push state)) instrs
  | PUSHACC4 ->
      compile code limit (pc + 1) (State.acc 4 (State.push state)) instrs
  | PUSHACC5 ->
      compile code limit (pc + 1) (State.acc 5 (State.push state)) instrs
  | PUSHACC6 ->
      compile code limit (pc + 1) (State.acc 6 (State.push state)) instrs
  | PUSHACC7 ->
      compile code limit (pc + 1) (State.acc 7 (State.push state)) instrs
  | PUSHACC ->
      let n = getu code (pc + 1) in
      compile code limit (pc + 2) (State.acc n (State.push state)) instrs
  | POP ->
      let n = getu code (pc + 1) in
      compile code limit (pc + 2) (State.pop n state) instrs
  | ASSIGN ->
      let n = getu code (pc + 1) in
      let y = State.peek n state in
      let z = State.accu state in
      if debug then Format.printf "%a = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 2) state
        (Let (x, Const 0) :: Assign (y, z) :: instrs)
  | ENVACC1 ->
      compile code limit (pc + 1) (State.env_acc 1 state) instrs
  | ENVACC2 ->
      compile code limit (pc + 1) (State.env_acc 2 state) instrs
  | ENVACC3 ->
      compile code limit (pc + 1) (State.env_acc 3 state) instrs
  | ENVACC4 ->
      compile code limit (pc + 1) (State.env_acc 4 state) instrs
  | ENVACC ->
      let n = getu code (pc + 1) in
      compile code limit (pc + 2) (State.env_acc n state) instrs
  | PUSHENVACC1 ->
      compile code limit (pc + 1) (State.env_acc 1 (State.push state)) instrs
  | PUSHENVACC2 ->
      compile code limit (pc + 1) (State.env_acc 2 (State.push state)) instrs
  | PUSHENVACC3 ->
      compile code limit (pc + 1) (State.env_acc 3 (State.push state)) instrs
  | PUSHENVACC4 ->
      compile code limit (pc + 1) (State.env_acc 4 (State.push state)) instrs
  | PUSHENVACC ->
      let n = getu code (pc + 1) in
      compile code limit (pc + 2) (State.env_acc n (State.push state)) instrs
  | PUSH_RETADDR ->
      compile code limit (pc + 2)
        {state with State.stack =
            State.Dummy :: State.Dummy :: State.Dummy :: state.State.stack}
        instrs
  | APPLY ->
      let n = getu code (pc + 1) in
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab n state in
      if debug then begin
        Format.printf "%a = %a(" Var.print x Var.print f;
        for i = 0 to n - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i)
        done;
        Format.printf ")@."
      end;
      compile code limit (pc + 2) (State.pop 3 state)
        (Let (x, Apply (f, args)) :: instrs)
  | APPLY1 ->
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let y = State.peek 0 state in
      if debug then
        Format.printf "%a = %a(%a)@." Var.print x
          Var.print f Var.print y;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Apply (f, [y])) :: instrs)
  | APPLY2 ->
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let y = State.peek 0 state in
      let z = State.peek 1 state in
      if debug then Format.printf "%a = %a(%a, %a)@." Var.print x
        Var.print f Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 2 state)
        (Let (x, Apply (f, [y; z])) :: instrs)
  | APPLY3 ->
      let f = State.accu state in
      let (x, state) = State.fresh_var state in
      let y = State.peek 0 state in
      let z = State.peek 1 state in
      let t = State.peek 2 state in
      if debug then Format.printf "%a = %a(%a, %a, %a)@." Var.print x
        Var.print f Var.print y Var.print z Var.print t;
      compile code limit (pc + 1) (State.pop 3 state)
        (Let (x, Apply (f, [y; z; t])) :: instrs)
  | APPTERM ->
      let n = getu code (pc + 1) in
      let f = State.accu state in
      let (l, state) = State.grab n state in
      if debug then begin
        Format.printf "return %a(" Var.print f;
        for i = 0 to n - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth l i)
        done;
        Format.printf ")@."
      end;
      let (x, state) = State.fresh_var state in
      (Let (x, Apply (f, l)) :: instrs, Return x, state)
  | APPTERM1 ->
      let f = State.accu state in
      let x = State.peek 0 state in
      if debug then Format.printf "return %a(%a)@." Var.print f Var.print x;
      let (y, state) = State.fresh_var state in
      (Let (y, Apply (f, [x])) :: instrs, Return y, state)
  | APPTERM2 ->
      let f = State.accu state in
      let x = State.peek 0 state in
      let y = State.peek 1 state in
      if debug then Format.printf "return %a(%a, %a)@."
        Var.print f Var.print x Var.print y;
      let (z, state) = State.fresh_var state in
      (Let (z, Apply (f, [x; y])) :: instrs, Return z, state)
  | APPTERM3 ->
      let f = State.accu state in
      let x = State.peek 0 state in
      let y = State.peek 1 state in
      let z = State.peek 2 state in
      if debug then Format.printf "return %a(%a, %a, %a)@."
        Var.print f Var.print x Var.print y Var.print z;
      let (t, state) = State.fresh_var state in
      (Let (t, Apply (f, [x; y; z])) :: instrs, Return t, state)
  | RETURN ->
      let x = State.accu state in
      if debug then Format.printf "return %a@." Var.print x;
      (instrs, Return x, state)
  | RESTART ->
      assert false
  | GRAB ->
      compile code limit (pc + 2) state instrs
  | CLOSURE ->
      let nvars = getu code (pc + 1) in
      let addr = pc + gets code (pc + 2) + 2 in
      let state = if nvars > 0 then State.push state else state in
      let (vals, state) = State.grab nvars state in
      let (x, state) = State.fresh_var state in
      let (s, state) = State.fresh_var state in
      let (y, state) = State.fresh_var state in
      let env =
        Array.of_list (State.Dummy :: List.map (fun x -> State.Var x) vals) in
      if debug then Format.printf "fun %a (" Var.print x;
      let nparams =
        match (get_instr code addr).code with
          GRAB -> getu code (addr + 1) + 1
        | _    -> 1
      in
      let state' = State.start_function state env 0 in
      let rec make_stack i state =
        if i = nparams then ([], state) else begin
          let (x, state) = State.fresh_var state in
          let (params, state) =
            make_stack (i + 1) (State.push state) in
          if debug then if i < nparams - 1 then Format.printf ", ";
          if debug then Format.printf "%a" Var.print x;
          (x :: params, state)
        end
      in
      let (params, state') = make_stack 0 state' in
      if debug then Format.printf ") {@.";
      let state' = State.clear_accu state' in
      compile_block code (-1) addr state';
      if debug then Format.printf "}@.";
      compile code limit (pc + 3) state
        (Let (y, Block (Obj.closure_tag, [|x; s|])) ::
         Let (s, Const (List.length params)) ::
         Let (x, Closure (List.rev params, addr)) :: instrs)
  | CLOSUREREC ->
      let nfuncs = getu code (pc + 1) in
      let nvars = getu code (pc + 2) in
      let state = if nvars > 0 then (State.push state) else state in
      let (vals, state) = State.grab nvars state in
      let state = ref state in
      let vars = ref [] in
      for i = 0 to nfuncs - 1 do
        let (x, st) = State.fresh_var !state in
        let (s, st) = State.fresh_var st in
        let (y, st) = State.fresh_var st in
        vars := (i, (x, s, y)) :: !vars;
        state := State.push st
      done;
      let env = ref (List.map (fun x -> State.Var x) vals) in
      List.iter
        (fun (i, (_, _, y)) ->
           env := State.Var y :: !env;
           if i > 0 then env := State.Dummy :: !env)
        !vars;
      let env = Array.of_list !env in
      let state = !state in
      let instrs =
        List.fold_left
          (fun instr (i, (x, s, y)) ->
             let addr = pc + 3 + gets code (pc + 3 + i) in
             if debug then Format.printf "fun %a (" Var.print y;
             let nparams =
               match (get_instr code addr).code with
                 GRAB -> getu code (addr + 1) + 1
               | _    -> 1
             in
             let state' = State.start_function state env (i * 2) in
             let rec make_stack i state =
               if i = nparams then ([], state) else begin
                 let (x, state) = State.fresh_var state in
                 let (params, state) =
                   make_stack (i + 1) (State.push state) in
                 if debug then if i < nparams - 1 then Format.printf ", ";
                 if debug then Format.printf "%a" Var.print x;
                 (x :: params, state)
               end
             in
             let (params, state') = make_stack 0 state' in
             if debug then Format.printf ") {@.";
             let state' = State.clear_accu state' in
             compile_block code (-1) addr state';
             if debug then Format.printf "}@.";
             Let (y, Block (Obj.closure_tag, [|x; s|])) ::
             Let (s, Const (List.length params)) ::
             Let (x, Closure (List.rev params, addr)) ::
             instr)
         instrs (List.rev !vars)
      in
      compile code limit (pc + 3 + nfuncs) (State.acc (nfuncs - 1) state) instrs
  | OFFSETCLOSUREM2 ->
      compile code limit (pc + 1) (State.env_acc (-2) state) instrs
  | OFFSETCLOSURE0 ->
      compile code limit (pc + 1) (State.env_acc 0 state) instrs
  | OFFSETCLOSURE2 ->
      compile code limit (pc + 1) (State.env_acc 2 state) instrs
  | OFFSETCLOSURE ->
      let n = gets code (pc + 1) in
      compile code limit (pc + 2) (State.env_acc n state) instrs
  | PUSHOFFSETCLOSUREM2 ->
      let state = State.push state in
      compile code limit (pc + 1) (State.env_acc (-2) state) instrs
  | PUSHOFFSETCLOSURE0 ->
      let state = State.push state in
      compile code limit (pc + 1) (State.env_acc 0 state) instrs
  | PUSHOFFSETCLOSURE2 ->
      let state = State.push state in
      compile code limit (pc + 1) (State.env_acc 2 state) instrs
  | PUSHOFFSETCLOSURE ->
      let state = State.push state in
      let n = gets code (pc + 1) in
      compile code limit (pc + 2) (State.env_acc n state) instrs
  | GETGLOBAL ->
      let i = getu code (pc + 1) in
      let state =
        let g = State.globals state in
        match g.vars.(i) with
          Some x ->
            if debug then Format.printf "(global access %a)@." Var.print x;
            State.set_accu state x
        | None ->
            g.is_const.(i) <- true;
            let (x, state) = State.fresh_var state in
            if debug then Format.printf "%a = CONST(%d)@." Var.print x i;
            g.vars.(i) <- Some x;
            state
      in
      compile code limit (pc + 2) state instrs
  | PUSHGETGLOBAL ->
      let state = State.push state in
      let i = getu code (pc + 1) in
      let state =
        let g = State.globals state in
        match g.vars.(i) with
          Some x ->
            if debug then Format.printf "(global access %a)@." Var.print x;
            State.set_accu state x
        | None ->
            g.is_const.(i) <- true;
            let (x, state) = State.fresh_var state in
            if debug then Format.printf "%a = CONST(%d)@." Var.print x i;
            g.vars.(i) <- Some x;
            state
      in
      compile code limit (pc + 2) state instrs
  | GETGLOBALFIELD ->
      let i = getu code (pc + 1) in
      let (x, state) =
        let g = State.globals state in
        match g.vars.(i) with
          Some x ->
            if debug then Format.printf "(global access %a)@." Var.print x;
            (x, State.set_accu state x)
        | None ->
            g.is_const.(i) <- true;
            let (x, state) = State.fresh_var state in
            if debug then Format.printf "%a = CONST(%d)@." Var.print x i;
            g.vars.(i) <- Some x;
            (x, state)
      in
      let j = getu code (pc + 2) in
      let (y, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
      compile code limit (pc + 3) state (Let (y, Field (x, j)) :: instrs)
  | PUSHGETGLOBALFIELD ->
      let state = State.push state in
      let i = getu code (pc + 1) in
      let (x, state) =
        let g = State.globals state in
        match g.vars.(i) with
          Some x ->
            if debug then Format.printf "(global access %a)@." Var.print x;
            (x, State.set_accu state x)
        | None ->
            g.is_const.(i) <- true;
            let (x, state) = State.fresh_var state in
            if debug then Format.printf "%a = CONST(%d)@." Var.print x i;
            g.vars.(i) <- Some x;
            (x, state)
      in
      let j = getu code (pc + 2) in
      let (y, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a[%d]@." Var.print y Var.print x j;
      compile code limit (pc + 3) state (Let (y, Field (x, j)) :: instrs)
  | SETGLOBAL ->
      let i = getu code (pc + 1) in
      let y = State.accu state in
      let g = State.globals state in
      assert (g.vars.(i) = None);
      if debug then Format.printf "(global %d) = %a@." i Var.print y;
      g.vars.(i) <- Some y;
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 2) state (Let (x, Const 0) :: instrs)
  | ATOM0 ->
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = ATOM(0)@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Block (0, [||])) :: instrs)
  | ATOM ->
      let i = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = ATOM(%d)@." Var.print x i;
      compile code limit (pc + 2) state (Let (x, Block (i, [||])) :: instrs)
  | PUSHATOM0 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = ATOM(0)@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Block (0, [||])) :: instrs)
  | PUSHATOM ->
      let state = State.push state in
      let i = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = ATOM(%d)@." Var.print x i;
      compile code limit (pc + 2) state (Let (x, Block (i, [||])) :: instrs)
  | MAKEBLOCK ->
      let size = getu code (pc + 1) in
      let tag = getu code (pc + 2) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (contents, state) = State.grab size state in
      if debug then begin
        Format.printf "%a = { " Var.print x;
        for i = 0 to size - 1 do
          Format.printf "%d = %a; " i Var.print (List.nth contents i);
        done;
        Format.printf "}@."
      end;
      compile code limit (pc + 3) state
        (Let (x, Block (tag, Array.of_list contents)) :: instrs)
  | MAKEBLOCK1 ->
      let tag = getu code (pc + 1) in
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = { 0 = %a; }@." Var.print x Var.print y;
      compile code limit (pc + 2) state (Let (x, Block (tag, [|y|])) :: instrs)
  | MAKEBLOCK2 ->
      let tag = getu code (pc + 1) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = { 0 = %a; 1 = %a; }@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 2) (State.pop 1 state)
        (Let (x, Block (tag, [|y; z|])) :: instrs)
  | MAKEBLOCK3 ->
      let tag = getu code (pc + 1) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let t = State.peek 1 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = { 0 = %a; 1 = %a; 2 = %a }@."
        Var.print x Var.print y Var.print z Var.print t;
      compile code limit (pc + 2) (State.pop 2 state)
        (Let (x, Block (tag, [|y; z; t|])) :: instrs)
  | MAKEFLOATBLOCK ->
      let size = getu code (pc + 1) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (contents, state) = State.grab size state in
      if debug then begin
        Format.printf "%a = { " Var.print x;
        for i = 0 to size - 1 do
          Format.printf "%d = %a; " i Var.print (List.nth contents i);
        done;
        Format.printf "}@."
      end;
      compile code limit (pc + 2) state
        (Let (x, Block (-1, Array.of_list contents)) :: instrs)
  | GETFIELD0 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a[0]@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Field (y, 0)) :: instrs)
  | GETFIELD1 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a[1]@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Field (y, 1)) :: instrs)
  | GETFIELD2 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a[2]@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Field (y, 2)) :: instrs)
  | GETFIELD3 ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a[3]@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Field (y, 3)) :: instrs)
  | GETFIELD ->
      let y = State.accu state in
      let n = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
      compile code limit (pc + 2) state (Let (x, Field (y, n)) :: instrs)
  | GETFLOATFIELD ->
      let y = State.accu state in
      let n = getu code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a[%d]@." Var.print x Var.print y n;
      compile code limit (pc + 2) state (Let (x, Field (y, n)) :: instrs)
  | SETFIELD0 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug then Format.printf "%a[0] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, 0, z) :: instrs)
  | SETFIELD1 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug then Format.printf "%a[1] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, 1, z) :: instrs)
  | SETFIELD2 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug then Format.printf "%a[2] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, 2, z) :: instrs)
  | SETFIELD3 ->
      let y = State.accu state in
      let z = State.peek 0 state in
      if debug then Format.printf "%a[3] = %a@." Var.print y Var.print z;
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, 3, z) :: instrs)
  | SETFIELD ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let n = getu code (pc + 1) in
      if debug then Format.printf "%a[%d] = %a@." Var.print y n Var.print z;
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 2) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, n, z) :: instrs)
  | SETFLOATFIELD ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let n = getu code (pc + 1) in
      if debug then Format.printf "%a[%d] = %a@." Var.print y n Var.print z;
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 2) (State.pop 1 state)
        (Let (x, Const 0) :: Set_field (y, n, z) :: instrs)
  | VECTLENGTH ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a.length@."
        Var.print x Var.print y;
      compile code limit (pc + 1) state
        (Let (x, Prim (Vectlength, [y])) :: instrs)
  | GETVECTITEM ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a[%a]@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Array_get, [y; z])) :: instrs)
  | SETVECTITEM ->
      if debug then Format.printf "%a[%a] = %a@." Var.print (State.accu state)
        Var.print (State.peek 0 state)
        Var.print (State.peek 1 state);
      let instrs =
        Array_set (State.accu state, State.peek 0 state, State.peek 1 state)
        :: instrs
      in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 2 state)
        (Let (x, Const 0) :: instrs)
  | GETSTRINGCHAR ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a[%a]@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Array_get, [y; z])) :: instrs)
  | SETSTRINGCHAR ->
      if debug then Format.printf "%a[%a] = %a@." Var.print (State.accu state)
        Var.print (State.peek 0 state)
        Var.print (State.peek 1 state);
      let instrs =
        Array_set (State.accu state, State.peek 0 state, State.peek 1 state)
        :: instrs
      in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) (State.pop 2 state)
        (Let (x, Const 0) :: instrs)
  | BRANCH ->
      let offset = gets code (pc + 1) in
      if debug then Format.printf "... (branch)@.";
      (instrs, Branch (pc + offset + 1, State.opt_accu state), state)
  | BRANCHIF ->
      let offset = gets code (pc + 1) in
      let x = State.accu state in
      (instrs,
       Cond (IsTrue, x, (pc + offset + 1, Some x), (pc + 2, Some x)), state)
  | BRANCHIFNOT ->
      let offset = gets code (pc + 1) in
      let x = State.accu state in
      (instrs,
       Cond (IsTrue, x, (pc + 2, Some x), (pc + offset + 1, Some x)), state)
  | SWITCH ->
if debug then Format.printf "switch ...@.";
      let sz = getu code (pc + 1) in
      let x = State.accu state in
      let l = sz land 0xFFFF in
      let it =
        Array.init (sz land 0XFFFF)
          (fun i -> (pc + 2 + gets code (pc + 2 + i), Some x))
      in
      let bt =
        Array.init (sz lsr 16)
          (fun i -> (pc + 2 + gets code (pc + 2 + l + i), Some x))
      in
      (instrs, Switch (x, it, bt), state)
  | BOOLNOT ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = !%a@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Prim (Not, [y])) :: instrs)
  | PUSHTRAP ->
      let addr = pc + 1 + gets code (pc + 1) in
      let (_, state') = State.fresh_var state in
      compile_block code (-1) addr state';
      compile_block code (-1) (pc + 2)
        {state with State.stack =
            State.Dummy :: State.Dummy :: State.Dummy :: State.Dummy ::
            state.State.stack};
      (instrs, Pushtrap ((pc + 2, State.opt_accu state), addr), state)
  | POPTRAP ->
      compile code limit (pc + 1) (State.pop 4 state) (Poptrap :: instrs)
  | RAISE ->
      if debug then Format.printf "throw(%a)@." Var.print (State.accu state);
      (instrs, Raise (State.accu state), state)
  | CHECK_SIGNALS ->
      compile code limit (pc + 1) state instrs
  | C_CALL1 ->
      let prim = primitive_name state (getu code (pc + 1)) in
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = ccall \"%s\" (%a)@."
        Var.print x prim Var.print y;
      compile code limit (pc + 2) state
        (Let (x, Prim (C_call prim, [y])) :: instrs)
  | C_CALL2 ->
      let prim = primitive_name state (getu code (pc + 1)) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = ccall \"%s\" (%a, %a)@."
        Var.print x prim Var.print y Var.print z;
      compile code limit (pc + 2) (State.pop 1 state)
        (Let (x, Prim (C_call prim, [y; z])) :: instrs)
  | C_CALL3 ->
      let prim = primitive_name state (getu code (pc + 1)) in
      let y = State.accu state in
      let z = State.peek 0 state in
      let t = State.peek 1 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = ccall \"%s\" (%a, %a, %a)@."
        Var.print x prim Var.print y Var.print z Var.print t;
      compile code limit (pc + 2) (State.pop 2 state)
        (Let (x, Prim (C_call prim, [y; z; t])) :: instrs)
  | C_CALL4 ->
      let nargs = 4 in
      let prim = primitive_name state (getu code (pc + 1)) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab nargs state in
      if debug then begin
        Format.printf "%a = ccal \"%s\" (" Var.print x prim;
        for i = 0 to nargs - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i);
        done;
        Format.printf ")@."
      end;
      compile code limit (pc + 2) state
        (Let (x, Prim (C_call prim, args)) :: instrs)
  | C_CALL5 ->
      let nargs = 5 in
      let prim = primitive_name state (getu code (pc + 1)) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab nargs state in
      if debug then begin
        Format.printf "%a = ccal \"%s\" (" Var.print x prim;
        for i = 0 to nargs - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i);
        done;
        Format.printf ")@."
      end;
      compile code limit (pc + 2) state
        (Let (x, Prim (C_call prim, args)) :: instrs)
  | C_CALLN ->
      let nargs = getu code (pc + 1) in
      let prim = primitive_name state (getu code (pc + 2)) in
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      let (args, state) = State.grab nargs state in
      if debug then begin
        Format.printf "%a = ccal \"%s\" (" Var.print x prim;
        for i = 0 to nargs - 1 do
          if i > 0 then Format.printf ", ";
          Format.printf "%a" Var.print (List.nth args i);
        done;
        Format.printf ")@."
      end;
      compile code limit (pc + 3) state
        (Let (x, Prim (C_call prim, args)) :: instrs)
  | CONST0 ->
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 0) :: instrs)
  | CONST1 ->
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 1@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 1) :: instrs)
  | CONST2 ->
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 2@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 2) :: instrs)
  | CONST3 ->
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 3@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 3) :: instrs)
  | CONSTINT ->
      let n = gets code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %d@." Var.print x n;
      compile code limit (pc + 2) state (Let (x, Const n) :: instrs)
  | PUSHCONST0 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 0@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 0) :: instrs)
  | PUSHCONST1 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 1@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 1) :: instrs)
  | PUSHCONST2 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 2@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 2) :: instrs)
  | PUSHCONST3 ->
      let state = State.push state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = 3@." Var.print x;
      compile code limit (pc + 1) state (Let (x, Const 3) :: instrs)
  | PUSHCONSTINT ->
      let state = State.push state in
      let n = gets code (pc + 1) in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %d@." Var.print x n;
      compile code limit (pc + 2) state (Let (x, Const n) :: instrs)
  | NEGINT ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = -%a@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Prim (Neg, [y])) :: instrs)
  | ADDINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a + %a@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Add, [y; z])) :: instrs)
  | SUBINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a - %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Sub, [y; z])) :: instrs)
  | MULINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a * %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Mul, [y; z])) :: instrs)
  | DIVINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a / %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Div, [y; z])) :: instrs)
  | MODINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a %% %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Mod, [y; z])) :: instrs)
  | ANDINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a & %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (And, [y; z])) :: instrs)
  | ORINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a | %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Or, [y; z])) :: instrs)
  | XORINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a ^ %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Xor, [y; z])) :: instrs)
  | LSLINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a << %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Lsl, [y; z])) :: instrs)
  | LSRINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a >>> %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Lsr, [y; z])) :: instrs)
  | ASRINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a >> %a@." Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Asr, [y; z])) :: instrs)
  | EQ ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = mk_bool(%a == %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Eq, [y; z])) :: instrs)
  | NEQ ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = mk_bool(%a != %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Neq, [y; z])) :: instrs)
  | LTINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = mk_bool(%a < %a)@." Var.print x
        Var.print y Var.print (State.peek 0 state);
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Lt, [y; z])) :: instrs)
  | LEINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = mk_bool(%a <= %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Le, [y; z])) :: instrs)
  | GTINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = mk_bool(%a > %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Le, [z; y])) :: instrs)
  | GEINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = mk_bool(%a >= %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Lt, [z; y])) :: instrs)
  | OFFSETINT ->
      let n = gets code (pc + 1) in
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = %a + %d@." Var.print x Var.print y n;
      compile code limit (pc + 2) state
        (Let (x, Prim (Offset n, [y])) :: instrs)
  | OFFSETREF ->
      let n = gets code (pc + 1) in
      let x = State.accu state in
      if debug then Format.printf "%a += %d@." Var.print x n;
      let instrs = Offset_ref (x, n) :: instrs in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "x = 0@.";
      compile code limit (pc + 2) state (Let (x, Const 0) :: instrs)
  | ISINT ->
      let y = State.accu state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = !%a@." Var.print x Var.print y;
      compile code limit (pc + 1) state (Let (x, Prim (IsInt, [y])) :: instrs)
  | BEQ ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      (instrs,
       Cond (CEq n, x, (pc + offset + 2, Some x), (pc + 3, Some x)), state)
  | BNEQ ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      (instrs,
       Cond (CEq n, x, (pc + 3, Some x), (pc + offset + 2, Some x)), state)
  | BLTINT ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      (instrs,
       Cond (CLt n, x, (pc + offset + 2, Some x), (pc + 3, Some x)), state)
  | BLEINT ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      (instrs,
       Cond (CLe n, x, (pc + offset + 2, Some x), (pc + 3, Some x)), state)
  | BGTINT ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      (instrs,
       Cond (CLe n, x, (pc + 3, Some x), (pc + offset + 2, Some x)), state)
  | BGEINT ->
      let n = gets code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      (instrs,
       Cond (CLt n, x, (pc + 3, Some x), (pc + offset + 2, Some x)), state)
  | BULTINT ->
      let n = getu code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      (instrs,
       Cond (CUlt n, x, (pc + offset + 2, Some x), (pc + 3, Some x)), state)
  | BUGEINT ->
      let n = getu code (pc + 1) in
      let offset = gets code (pc + 2) in
      let x = State.accu state in
      (instrs,
       Cond (CUlt n, x, (pc + 3, Some x), (pc + offset + 2, Some x)), state)
  | ULTINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = mk_bool(%a < %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Ult, [y; z])) :: instrs)
  | UGEINT ->
      let y = State.accu state in
      let z = State.peek 0 state in
      let (x, state) = State.fresh_var state in
      if debug then Format.printf "%a = mk_bool(%a >= %a)@."
        Var.print x Var.print y Var.print z;
      compile code limit (pc + 1) (State.pop 1 state)
        (Let (x, Prim (Ult, [z; y])) :: instrs)
(* GETPUBMET GETDYNMET GETMETHOD *)
  | GETPUBMET ->
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
if debug then prerr_endline "XXX";
      let state = State.push state in
      compile code limit (pc + 3) state instrs
  | STOP ->
      (instrs, Stop, state)
  | _ ->
if debug then prerr_endline "XXX";
      compile code limit (pc + 1) state instrs
  end

(****)

let parse_bytecode code state =
  let cont = analyse_blocks code in
ignore cont;
  compile_block code (-1) 0 state;

  let compiled_block =
    IntMap.mapi
      (fun pc (state, instr, last) ->
         (State.opt_accu state, instr, last))
      !compiled_block
  in
(*
  IntMap.iter
    (fun pc _ ->
       Format.eprintf "==== %d ====@." pc;
       let l = try IntMap.find pc !start_state with Not_found -> [] in
       List.iter State.print l;
       match l with
         s :: r -> assert (List.for_all (fun s' -> s.State.stack = s'.State.stack) r)
         | [] -> ())
    cont
*)

  let g = State.globals state in
  let l = ref [] in
  for i = Array.length g.constants - 1  downto 0 do
    match g.vars.(i) with
      Some x when g.is_const.(i) ->
        l := Let (x, Constant g.constants.(i)) :: !l
    | _ ->
        ()
  done;
  let last = Branch (0, None) in
  let pc = String.length code / 4 in
  let compiled_block = IntMap.add pc (None, !l, last) compiled_block in
  (pc, compiled_block, pc + 1)

(****)

exception Bad_magic_number

let exec_magic_number = "Caml1999X008"

module Tbl = struct
  type ('a, 'b) t =
      Empty
    | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int

  let rec iter f = function
      Empty -> ()
    | Node(l, v, d, r, _) ->
        iter f l; f v d; iter f r
end

let seek_section toc ic name =
  let rec seek_sec curr_ofs = function
    [] -> raise Not_found
  | (n, len) :: rem ->
      if n = name
      then begin seek_in ic (curr_ofs - len); len end
      else seek_sec (curr_ofs - len) rem in
  seek_sec (in_channel_length ic - 16 - 8 * List.length toc) toc

let read_toc ic =
  let pos_trailer = in_channel_length ic - 16 in
  seek_in ic pos_trailer;
  let num_sections = input_binary_int ic in
  let header = String.create(String.length exec_magic_number) in
  really_input ic header 0 (String.length exec_magic_number);
  if header <> exec_magic_number then raise Bad_magic_number;
  seek_in ic (pos_trailer - 8 * num_sections);
  let section_table = ref [] in
  for i = 1 to num_sections do
    let name = String.create 4 in
    really_input ic name 0 4;
    let len = input_binary_int ic in
    section_table := (name, len) :: !section_table
  done;
  !section_table

let read_primitive_table toc ic =
  let len = seek_section toc ic "PRIM" in
  let p = String.create len in
  really_input ic p 0 len;
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

let _ =
  let ic = stdin in
  let toc = read_toc ic in
  let primitives = read_primitive_table toc ic in
  let code_size = seek_section toc ic "CODE" in
(*  Format.eprintf "%d@." code_size;*)
  let code = String.create code_size in
  really_input ic code 0 code_size;

  ignore(seek_section toc ic "DATA");
  let init_data = (input_value ic : Obj.t array) in

  let globals =
    { vars = Array.create (Array.length init_data) None;
      is_const = Array.create (Array.length init_data) false;
      constants = init_data;
      primitives = primitives } in

(*
  Format.eprintf "Constantes: %d@." (Array.length init_data);
  for i = 0 to Array.length init_data - 1 do
    Format.eprintf "%d ==> %a@." i print_obj init_data.(i)
  done;
  ignore (seek_section ic "SYMB");
  let (_, sym_table) = (input_value ic : int * ((*Ident.t*)unit, int) Tbl.t) in
Format.eprintf "Globals:";
  Tbl.iter (fun id pos -> Format.eprintf " %d" pos)
sym_table;
Format.eprintf "@.";
*)
  let state = State.initial globals in
  let p = parse_bytecode code state in

(*
  print_program (fun _ _ -> "") p;
*)
Format.eprintf "Data flow@.";
  let (p, approx) = Flow.f p in
Format.eprintf "Dead-code@.";
  let (p, _) = Deadcode.f p in
Format.eprintf "Control flow simplifications@.";
  let p = Control.simpl p in
Format.eprintf "Dead-code@.";
  let (p, live_vars) = Deadcode.f p in


(*

  let p = Control.simpl p in

  let p = Flow.f p in
  let p = Deadcode.f p in

  let p = Control.simpl p in
*)

  print_program (fun _ _ -> "") p;

Generate.f p live_vars
