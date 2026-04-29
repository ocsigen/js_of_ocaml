let raises_failure f =
  try
    let _ = f () in
    false
  with Failure _ -> true

let () =
  assert (raises_failure (fun () -> Int64.of_string ""));
  assert (raises_failure (fun () -> Int64.of_string "+"));
  assert (raises_failure (fun () -> Int64.of_string "-"));
  assert (raises_failure (fun () -> Int64.of_string "0x"));
  assert (raises_failure (fun () -> Int64.of_string "0o"));
  assert (raises_failure (fun () -> Int64.of_string "0b"));
  assert (raises_failure (fun () -> Int64.of_string "0u"));
  assert (raises_failure (fun () -> Int64.of_string "+0x"));
  assert (raises_failure (fun () -> Int64.of_string "-0b"));
  assert (Int64.equal (Int64.of_string "42") 42L);
  assert (Int64.equal (Int64.of_string "-42") (-42L));
  assert (Int64.equal (Int64.of_string "0x2a") 42L)
