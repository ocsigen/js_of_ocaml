let construct x = [| x |]

let project (x : float array) = x.(0)

let _ = project (construct 4.0)
