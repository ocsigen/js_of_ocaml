include Sizable_int.Make (struct
  let name = "Targetint"

  let allowed_sizes = [| 63; 32; 31 |]
end)
