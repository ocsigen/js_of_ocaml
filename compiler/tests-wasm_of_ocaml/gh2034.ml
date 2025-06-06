let f x = x#b

let o1 =
  object
    method a = ()

    method b = ()
  end

let o2 =
  object
    method b = ()
  end

let () =
  f o1;
  f o2
