(* TEST *)

let r = (Atomic.make [@ocaml.alert "-unsafe_multidomain"]) 1
let () = assert ((Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r = 1)

let () = (Atomic.set [@ocaml.alert "-unsafe_multidomain"]) r 2
let () = assert ((Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r = 2)

let () = assert ((Atomic.exchange [@ocaml.alert "-unsafe_multidomain"]) r 3 = 2)

let () = assert ((Atomic.compare_and_set [@ocaml.alert "-unsafe_multidomain"]) r 3 4 = true)
let () = assert ((Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r = 4)

let () = assert ((Atomic.compare_and_set [@ocaml.alert "-unsafe_multidomain"]) r 3 (-4) = false)
let () = assert ((Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r = 4 )

let () = assert ((Atomic.compare_and_set [@ocaml.alert "-unsafe_multidomain"]) r 3 4 = false)

let () = assert (Atomic.fetch_and_add r 2 = 4)
let () = assert ((Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r = 6)

let () = assert (Atomic.fetch_and_add r (-2) = 6)
let () = assert ((Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r = 4)

let () = assert ((Atomic.incr r; (Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r) = 5)

let () = assert ((Atomic.decr r; (Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r) = 4)

let () =
  let r = (Atomic.make [@ocaml.alert "-unsafe_multidomain"]) 0 in
  let cur = (Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r in
  ignore ((Atomic.set [@ocaml.alert "-unsafe_multidomain"]) r (cur + 1), (Atomic.set [@ocaml.alert "-unsafe_multidomain"]) r (cur - 1));
  assert ((Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r <> cur)

let () =
  let r = (Atomic.make [@ocaml.alert "-unsafe_multidomain"]) 0 in
  let cur = (Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r in
  ignore (Atomic.incr r, Atomic.decr r);
  assert ((Atomic.get [@ocaml.alert "-unsafe_multidomain"]) r = cur)
