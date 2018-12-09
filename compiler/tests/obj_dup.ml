let () =
  let s = "Hello" in
  let s': string = Obj.obj (Obj.dup (Obj.repr s)) in
  assert (s = s');
  assert (s != s')

let () =
  let s = Bytes.of_string "Hello" in
  let s': bytes = Obj.obj (Obj.dup (Obj.repr s)) in
  assert (s = s');
  assert (s != s');
  Bytes.set s' 1 'a';
  assert (s <> s')
