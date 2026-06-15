(* Native renderer: draw the shared scene with the real X11 Graphics lib,
   dump the framebuffer (row 0 = top) as a binary PPM to argv.(1). *)
let () =
  Graphics.open_graph " 200x200";
  Gscene.Scene.draw ();
  let w = Graphics.size_x () and h = Graphics.size_y () in
  let m = Graphics.dump_image (Graphics.get_image 0 0 w h) in
  let buf = Buffer.create ((w * h * 3) + 32) in
  Buffer.add_string buf (Printf.sprintf "P6\n%d %d\n255\n" w h);
  for r = 0 to h - 1 do
    for c = 0 to w - 1 do
      let col = m.(r).(c) in
      let col = if col < 0 then 0xffffff else col in
      Buffer.add_char buf (Char.chr ((col lsr 16) land 0xff));
      Buffer.add_char buf (Char.chr ((col lsr 8) land 0xff));
      Buffer.add_char buf (Char.chr (col land 0xff))
    done
  done;
  let oc = open_out_bin Sys.argv.(1) in
  Buffer.output_buffer oc buf;
  close_out oc;
  Graphics.close_graph ()
