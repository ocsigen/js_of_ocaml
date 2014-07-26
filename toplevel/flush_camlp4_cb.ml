let _ = Camlp4.Register.iter_and_take_callbacks
    (fun (name, callback)-> callback ())
