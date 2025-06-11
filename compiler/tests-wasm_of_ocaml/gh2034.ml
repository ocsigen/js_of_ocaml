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

let g x = x#d

let o3 =
  object
    method a = ()

    method b = ()

    method c = ()

    method d = ()
  end

let o4 =
  object
    method b = ()

    method c = ()

    method d = ()
  end

let () =
  f o1;
  f o2;
  g o3;
  g o4
