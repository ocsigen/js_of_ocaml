let construct x = [| x |]

let get (x : float array) = x.(0)

let get_ (x : _ array) = x.(0)

let set (x : float array) e = x.(0) <- e

let set_ (x : _ array) e = x.(0) <- e

let a = construct 1.0

let _ = set a 2.0

let _ = assert (Float.equal (get a) 2.0)

let _ = assert (Float.equal (get_ a) 2.0)

let _ = set_ a 3.0

let _ = assert (Float.equal (get a) 3.0)

let _ = assert (Float.equal (get_ a) 3.0)

let b = [| 1.0 |]

let _ = set b 2.0

let _ = assert (Float.equal (get b) 2.0)

let _ = assert (Float.equal (get_ b) 2.0)

let _ = set_ b 3.0

let _ = assert (Float.equal (get b) 3.0)

let _ = assert (Float.equal (get_ b) 3.0)

let construct2 x = [| x; x |]

let c = construct2 1.

let _ = assert (Float.equal c.(0) 1. && Float.equal c.(1) 1.)

let _ = c.(1) <- 2.

let _ = assert (Array.length c = 2)

let _ = assert (Float.equal c.(0) 1. && Float.equal c.(1) 2.)
