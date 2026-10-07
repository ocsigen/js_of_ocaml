The range analysis removes the bound checks of the accesses whose index is
known to be valid, and only those. The accesses of `bound_checks.ml` are
checked at run time; here we check which checks are removed: all the checks
of `removed.ml`, and none of `kept.ml`. Only a single compilation unit is
compiled at a time, so that the result does not depend on the standard
library. The analysis is only performed from `--opt 2`.

  $ cat > removed.ml <<'EOF2'
  > let sum a =
  >   let r = ref 0 in
  >   for i = 0 to Array.length a - 1 do
  >     r := !r + a.(i)
  >   done;
  >   !r
  > 
  > let sum_float (a : float array) =
  >   let r = ref 0. and i = ref 0 in
  >   while !i < Array.length a do
  >     r := !r +. a.(!i);
  >     incr i
  >   done;
  >   !r
  > 
  > let count s =
  >   let n = ref 0 in
  >   for i = 0 to String.length s - 1 do
  >     if s.[i] = 'e' then incr n
  >   done;
  >   !n
  > 
  > let get a pos = if pos < 0 || pos >= Array.length a then 0 else a.(pos)
  > 
  > let iter f a =
  >   for i = 0 to Array.length a - 1 do
  >     f a.(i)
  >   done
  > 
  > let down a =
  >   let r = ref 0 in
  >   for i = Array.length a - 1 downto 0 do
  >     r := !r + a.(i)
  >   done;
  >   !r
  > 
  > let down_while s =
  >   let n = ref 0 and i = ref (String.length s - 1) in
  >   while !i >= 0 do
  >     if s.[!i] = 'e' then incr n;
  >     decr i
  >   done;
  >   !n
  > 
  > let ne_loop s =
  >   let rec loop i n =
  >     if i <> String.length s then loop (i + 1) (if s.[i] = 'a' then n + 1 else n) else n
  >   in
  >   loop 0 0
  > 
  > let ne_while a =
  >   let i = ref 0 and r = ref 0 in
  >   while !i <> Array.length a do
  >     r := !r + a.(!i);
  >     incr i
  >   done;
  >   !r
  > 
  > let masked a x = if x land 15 < Array.length a then a.(x land 15) else 0
  > 
  > type t = { data : int array }
  > 
  > let same_field n i =
  >   let get t i =
  >     let i = i land 7 in
  >     if i < Array.length t.data then t.data.(i) else 0
  >   in
  >   get { data = Array.make n 0 } i + get { data = Array.make (n + 1) 0 } i
  > 
  > let created n =
  >   let p = Array.make n 0 in
  >   for i = 0 to n - 1 do
  >     p.(i) <- i * i
  >   done;
  >   let b = Bytes.create n in
  >   for i = 0 to n - 1 do
  >     Bytes.set b i 'x'
  >   done;
  >   p, b
  > 
  > let table x =
  >   let t = Array.make 16 0 in
  >   t.(x land 15)
  > 
  > let hex x = "0123456789abcdef".[x land 15]
  > 
  > let literal k =
  >   let d = [| 1; 2; 3; 4 |] in
  >   d.(k land 3)
  > 
  > let last s = if String.length s > 0 then s.[String.length s - 1] else ' '
  > 
  > let before_last a =
  >   let n = Array.length a in
  >   if n - 2 >= 0 then a.(n - 2) else 0
  > 
  > let next_element a i =
  >   let i = i land 255 in
  >   if i < Array.length a - 1 then a.(i + 1) else 0
  > 
  > let first a = if Array.length a > 0 then a.(0) else 0
  > 
  > let third a = if Array.length a > 2 then a.(2) else 0
  > 
  > let first_char s = if String.length s > 0 then s.[0] else ' '
  > 
  > let four s i = if String.length s >= 4 then s.[i land 3] else ' '
  > 
  > let literal_element x = [| x; x + 1; x + 2 |].(1)
  > 
  > let created_element n =
  >   let a = Array.make ((n land 7) + 4) 0 in
  >   a.(3)
  > 
  > let unrolled a =
  >   let n = Array.length a in
  >   let r = ref 0 and i = ref 0 in
  >   while !i + 3 < n do
  >     r := !r + a.(!i) + a.(!i + 1) + a.(!i + 2) + a.(!i + 3);
  >     i := !i + 4
  >   done;
  >   while !i < n do
  >     r := !r + a.(!i);
  >     incr i
  >   done;
  >   !r
  > 
  > let unrolled_rec a =
  >   let n = Array.length a in
  >   let rec loop i r =
  >     if i + 1 < n then loop (i + 2) (r + a.(i) + a.(i + 1)) else r
  >   in
  >   loop 0 0
  > 
  > (* A literal array of 8 constants is a copy of a constant *)
  > let constants = [| 1l; 2l; 3l; 4l; 5l; 6l; 7l; 8l |]
  > 
  > let constant_table i = constants.(i land 7)
  > 
  > let count_board () =
  >   (* The arrays of arrays are local: exported, they could be modified *)
  >   let board = [| [| 0; 1; 1; 0 |]; [| 1; 1; 1; 1 |]; [| 0; 1; 1; 0 |] |] in
  >   let n = ref 0 in
  >   for i = 0 to 2 do
  >     for j = 0 to 3 do
  >       n := !n + board.(i).(j)
  >     done
  >   done;
  >   !n
  > 
  > let sum_table np =
  >   let float_tables =
  >     [| [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8. |]; [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9. |] |]
  >   in
  >   let t = float_tables.(np land 1) in
  >   let r = ref 0. in
  >   for k = 0 to 7 do
  >     r := !r +. t.(k)
  >   done;
  >   !r
  > 
  > let after_lt a =
  >   let n = Array.length a - 1 in
  >   if n >= 0
  >   then (
  >     let i = ref 0 in
  >     while !i < n do
  >       incr i
  >     done;
  >     a.(!i))
  >   else 0
  > 
  > let after_le a =
  >   let n = Array.length a - 2 in
  >   if n >= 0
  >   then (
  >     let i = ref 0 in
  >     while !i <= n do
  >       incr i
  >     done;
  >     a.(!i))
  >   else 0
  > 
  > let after_gt a =
  >   let j = ref (Array.length a - 1) in
  >   if !j >= 0
  >   then (
  >     while !j > 0 do
  >       decr j
  >     done;
  >     a.(!j))
  >   else 0
  > 
  > let bound_below_length a lo hi =
  >   if lo >= 0
  >   then
  >     if hi < Array.length a
  >     then
  >       if lo < hi
  >       then (
  >         let i = ref lo in
  >         while !i < hi do
  >           incr i
  >         done;
  >         a.(!i))
  >       else 0
  >     else 0
  >   else 0
  > 
  > let assigned_in_try f j =
  >   (* The values assigned to [i] are bounded by the conditions where they
  >      are assigned *)
  >   let a = Array.make 11 0 in
  >   let i = ref 0 in
  >   try
  >     if j >= 0 then if j < 10 then i := j;
  >     f a;
  >     0
  >   with Exit -> a.(!i + 1)
  > 
  > let assigned_index a f j =
  >   (* [i] is not assigned anymore once the handler is entered *)
  >   let i = ref 0 in
  >   try
  >     if j >= 0 then i := j;
  >     f ();
  >     0
  >   with Exit -> if !i < Array.length a then a.(!i) else 0
  > EOF2
  $ cat > kept.ml <<'EOF2'
  > let too_far a =
  >   let r = ref 0 in
  >   for i = 0 to Array.length a do
  >     r := !r + a.(i)
  >   done;
  >   !r
  > 
  > let other_array a b =
  >   let r = ref 0 in
  >   for i = 0 to Array.length a - 1 do
  >     r := !r + b.(i)
  >   done;
  >   !r
  > 
  > let string_too_far s pos = if pos <= String.length s then s.[pos] else ' '
  > 
  > let decremented a =
  >   let c = ref 0 in
  >   let dec = Sys.opaque_identity (fun () -> decr c) in
  >   dec ();
  >   if !c < Array.length a then a.(!c) else 0
  > 
  > let variable_bound a short =
  >   let r = ref 0 in
  >   let rec loop i =
  >     let arr = if i < 3 then a else short in
  >     let hi = Array.length arr - 1 in
  >     if i <> hi
  >     then (
  >       if i >= 0 then r := !r + arr.(i);
  >       loop (i + 1))
  >   in
  >   loop (-1);
  >   !r
  > 
  > let down_from_length a =
  >   let r = ref 0 in
  >   for i = Array.length a downto 0 do
  >     r := !r + a.(i)
  >   done;
  >   !r
  > 
  > let down_wrapping a n =
  >   (* [i] can wrap around: [i >= 0] at the access is not enough *)
  >   let r = ref 0 in
  >   let rec loop i n =
  >     if n > 0
  >     then (
  >       if i >= 0 then r := !r + a.(i);
  >       loop (i - 1) (n - 1))
  >   in
  >   loop (Array.length a - 1) n;
  >   !r
  > 
  > let ne_from_param s i0 =
  >   (* [i0] may be larger than the length *)
  >   let rec loop i n =
  >     if i <> String.length s then loop (i + 1) (if s.[i] = 'a' then n + 1 else n) else n
  >   in
  >   loop i0 0
  > 
  > let next s i =
  >   (* [i + 1] wraps around if [i] is [max_int] *)
  >   if i >= 0 && i + 1 < String.length s then s.[i + 1] else ' '
  > 
  > let masked_other a x = if x land 15 < Array.length a then a.(x land 31) else 0
  > 
  > type u = { mutable mdata : int array }
  > 
  > let mutable_field n i =
  >   let get t i =
  >     let i = i land 7 in
  >     if i < Array.length t.mdata
  >     then (
  >       t.mdata <- [||];
  >       t.mdata.(i))
  >     else 0
  >   in
  >   get { mdata = Array.make n 0 } i + get { mdata = Array.make (n + 1) 0 } i
  > 
  > let created_too_far n =
  >   let p = Array.make n 0 in
  >   for i = 0 to n do
  >     p.(i) <- i
  >   done;
  >   p
  > 
  > let table_too_far x =
  >   let t = Array.make 16 0 in
  >   t.(x land 31)
  > 
  > let hex_too_far x = "0123456789abcdef".[x land 31]
  > 
  > let next_element_too_far a i =
  >   let i = i land 255 in
  >   if i < Array.length a then a.(i + 1) else 0
  > 
  > let before_last_too_far a =
  >   let n = Array.length a in
  >   if n - 1 >= 0 then a.(n - 2) else 0
  > 
  > let guarded_bound a =
  >   (* The loop bound is only computed when [n > 10]: [i <= n - 1] does not
  >      hold when [a] is empty *)
  >   let n = Array.length a in
  >   let rec loop i r =
  >     let r = r + a.(i) in
  >     if n > 10
  >     then (
  >       let hi = n - 1 in
  >       if i <> hi then loop (i + 1) r else r)
  >     else r
  >   in
  >   loop 0 0
  > 
  > let fourth a = if Array.length a > 2 then a.(3) else 0
  > 
  > let eight s i = if String.length s >= 4 then s.[i land 7] else ' '
  > 
  > let unrolled_too_far a =
  >   let n = Array.length a in
  >   let r = ref 0 and i = ref 0 in
  >   while !i + 2 < n do
  >     r := !r + a.(!i + 3);
  >     incr i
  >   done;
  >   !r
  > 
  > let offset_wrapping a i =
  >   (* [i + 1] wraps around if [i] is [max_int] *)
  >   if i >= 0 && i + 1 < Array.length a then a.(i) else 0
  > 
  > let constants = [| 1l; 2l; 3l; 4l; 5l; 6l; 7l; 8l |]
  > 
  > let constant_table_too_far i = constants.(i land 15)
  > 
  > let offset_down_wrapping a i =
  >   (* [i - 1] wraps around if [i] is [min_int] *)
  >   if i < 50 then if i - 1 >= 0 then if i < Array.length a then a.(i) else 0 else 0
  >   else 0
  > 
  > let offset_not_dominating a i =
  >   (* The fact about [i + 1] only holds in one branch *)
  >   if i >= 0
  >   then
  >     let r = if i + 1 < Array.length a then 1 else 0 in
  >     r + a.(i)
  >   else 0
  > 
  > let unrolled_other_array a b =
  >   let r = ref 0 and i = ref 0 in
  >   while !i + 3 < Array.length a do
  >     r := !r + b.(!i + 3);
  >     i := !i + 4
  >   done;
  >   !r
  > 
  > let dup_unknown (a : int array) = (Obj.obj (Obj.dup (Obj.repr a)) : int array).(0)
  > 
  > let row_too_far i =
  >   let rows = [| [| 0; 1; 1; 0 |]; [| 1; 1; 1 |] |] in
  >   rows.(i land 3).(3)
  > 
  > let modified_row i j =
  >   let rows = [| [| 0; 1; 1; 0 |]; [| 1; 1; 1; 1 |] |] in
  >   rows.(i land 3) <- [||];
  >   rows.(j land 3).(3)
  > 
  > let row_of_param x i = [| x; [| 1; 2; 3; 4 |] |].(i land 3).(3)
  > 
  > let after_lt_empty a =
  >   (* The loop is not entered when [a] is empty *)
  >   let n = Array.length a - 1 in
  >   let i = ref 0 in
  >   while !i < n do
  >     incr i
  >   done;
  >   a.(!i)
  > 
  > let after_le_too_far a =
  >   let n = Array.length a - 1 in
  >   if n >= 0
  >   then (
  >     let i = ref 0 in
  >     while !i <= n do
  >       incr i
  >     done;
  >     a.(!i))
  >   else 0
  > 
  > let after_gt_empty a =
  >   let j = ref (Array.length a - 1) in
  >   while !j > 0 do
  >     decr j
  >   done;
  >   a.(!j)
  > 
  > let after_ge a =
  >   (* [j] is -1 after the loop *)
  >   let j = ref (Array.length a - 1) in
  >   if !j >= 0
  >   then (
  >     while !j >= 0 do
  >       decr j
  >     done;
  >     a.(!j))
  >   else 0
  > 
  > let bound_at_length a hi =
  >   (* [i <= hi <= Array.length a] *)
  >   if hi <= Array.length a
  >   then
  >     if hi > 0
  >     then (
  >       let i = ref 0 in
  >       while !i < hi do
  >         incr i
  >       done;
  >       a.(!i))
  >     else 0
  >   else 0
  > 
  > let mixed_guards a k =
  >   (* Incremented while [i <> n] or while [i <= n]: [i] may exceed [n] *)
  >   let n = Array.length a - 1 in
  >   if n >= 0
  >   then
  >     let rec loop i k =
  >       let x = a.(i) in
  >       if k > 0
  >       then if i <> n then loop (i + 1) (k - 1) else x
  >       else if i <= n
  >       then loop (i + 1) (k - 1)
  >       else x
  >     in
  >     loop 0 k
  >   else 0
  > 
  > let modified_by_blit i j =
  >   let rows = [| [| 0; 1; 1; 0 |]; [| 1; 1; 1; 1 |] |] in
  >   Array.blit [| [||] |] 0 rows (i land 1) 1;
  >   rows.(j land 3).(3)
  > 
  > let modified_by_closure i j =
  >   let rows = [| [| 0; 1; 1; 0 |]; [| 1; 1; 1; 1 |] |] in
  >   let f = Sys.opaque_identity (fun () -> rows.(i land 3) <- [||]) in
  >   f ();
  >   rows.(j land 3).(3)
  > 
  > let row_maybe_empty c j =
  >   let rows = [| (if c then [| 1; 2; 3; 4 |] else [||]) |] in
  >   rows.(j land 3).(3)
  > 
  > let assigned_in_try_too_far f j =
  >   let a = Array.make 11 0 in
  >   let i = ref 0 in
  >   try
  >     if j >= 0 then if j < 11 then i := j;
  >     f a;
  >     0
  >   with Exit -> a.(!i + 1)
  > 
  > let assigned_index_too_far a f j =
  >   let i = ref 0 in
  >   try
  >     if j >= 0 then i := j;
  >     f ();
  >     0
  >   with Exit -> if !i <= Array.length a then a.(!i) else 0
  > EOF2
  $ ocamlc -c removed.ml kept.ml
  $ wasm_of_ocaml compile --opt 2 --debug stats --debug bound-checks removed.cmo -o removed.wasmo 2>&1 | grep -c ': kept'
  0
  [1]
  $ wasm_of_ocaml compile --opt 2 --debug stats removed.cmo -o removed.wasmo 2>&1 | grep 'bound checks'
  Stats - bound checks removed: 35 arrays, 8 strings, 0 bigarrays
  $ wasm_of_ocaml compile --opt 2 --debug bound-checks kept.cmo -o kept.wasmo 2>&1 | grep -c ': removed'
  0
  [1]
  $ wasm_of_ocaml compile --opt 2 --debug bound-checks kept.cmo -o kept.wasmo 2>&1 | grep -c ': kept'
  48

  $ wasm_of_ocaml compile --debug stats removed.cmo -o removed.wasmo 2>&1 | grep 'bound checks'
  [1]
