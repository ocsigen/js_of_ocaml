let pi = 4. *. atan 1.

let deltay = 40_000. /. 360. /. 3600. *. 1000.

let deltax = deltay *. cos (44. *. pi /. 180.)

let precompute tile_height tile_width tile =
  let normals =
    Bigarray.(Array3.create Int8_signed C_layout) (tile_height - 2) (tile_width - 2) 3
  in
  let heights =
    Bigarray.(Array2.create Float32 C_layout) (tile_height - 2) (tile_width - 2)
  in
  for y = 1 to tile_height - 2 do
    for x = 1 to tile_width - 2 do
      let nx = (tile.{y, x - 1} -. tile.{y, x + 1}) *. deltay in
      let ny = (tile.{y - 1, x} -. tile.{y + 1, x}) *. deltax in
      let nz = 2. *. deltax *. deltay in
      let n = 127. /. sqrt ((nx *. nx) +. (ny *. ny) +. (nz *. nz)) in
      normals.{tile_height - 2 - y, x - 1, 0} <- truncate (nx *. n);
      normals.{tile_height - 2 - y, x - 1, 1} <- truncate (ny *. n);
      normals.{tile_height - 2 - y, x - 1, 2} <- truncate (nz *. n);
      heights.{tile_height - 2 - y, x - 1} <- tile.{y, x}
    done
  done

let tile_height = 1024

let tile_width = 1024

let tile = Bigarray.(Array2.create Float32 C_layout) tile_height tile_width

let () =
  for _ = 1 to 30 do
    precompute tile_height tile_width tile
  done
