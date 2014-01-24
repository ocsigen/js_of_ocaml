(* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Isaac Gouy, transliterated from Mike Pall's Lua program
*)

let fannkuch n =
  let p = Array.create n 0 in
  let q = Array.create n 0 in
  let s = Array.create n 0 in
  let sign = ref 1 in
  let maxflips = ref 0 in
  let sum = ref 0 in
  for i = 0 to n - 1 do
    p.(i) <- i; q.(i) <- i; s.(i) <- i
  done;
  while true do
    let q0 = ref p.(0) in
    if !q0 <> 0 then begin
       for i = 1 to n - 1 do
         q.(i) <- p.(i)
       done;
       let flips = ref 1 in
       while
          let qq = q.(!q0) in
          if qq = 0 then begin
             sum := !sum + !sign * !flips;
             if !flips > !maxflips then maxflips := !flips;
             false
          end else
            true
       do
         let qq = q.(!q0) in
         q.(!q0) <- !q0;
         if !q0 >= 3 then begin
           let i = ref 1 in
           let j = ref (!q0 - 1) in
           while
             let t = q.(!i) in
             q.(!i) <- q.(!j); q.(!j) <- t;
             incr i;
             decr j;
             !i < !j
           do () done
         end;
         q0 := qq; incr flips
       done
    end;
    if !sign = 1 then begin
       let t = p.(1) in p.(1) <- p.(0); p.(0) <- t; sign := -1
    end else begin
       let t = p.(1) in p.(1) <- p.(2); p.(2) <- t; sign := 1;
       try
         for i = 2 to n - 1 do
           let sx = s.(i) in
           if sx <> 0 then begin s.(i) <- sx - 1; raise Exit end;
           if i = n - 1 then begin
             Format.eprintf "%d %d@." !sum !maxflips; exit 0
           end;
           s.(i) <- i;
           let t = p.(0) in
           for j = 0 to i do p.(j) <- p.(j + 1) done;
           p.(i + 1) <- t
         done
       with Exit ->
         ()
    end
  done

let n = 10
let pf = fannkuch n
(*
//print(pf[0] + "\n" + "Pfannkuchen(" + n + ") = " + pf[1]);
*)
