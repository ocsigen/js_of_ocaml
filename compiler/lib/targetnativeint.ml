include Sizable_int.Make (struct
  let name = "Targetnativeint"

  let allowed_sizes = [| 64; 32 |]
end)
