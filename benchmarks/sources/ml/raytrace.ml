module Color = struct
  type t =
    { red : float
    ; green : float
    ; blue : float
    }

  let make r g b = { red = r; green = g; blue = b }

  (*
  let print ch c =
    let r = truncate (c.red *. 255.) in
    let g = truncate (c.green *. 255.) in
    let b = truncate (c.blue *. 255.) in
    Format.fprintf ch "rgb(%d,%d,%d)" r g b
*)

  let limit c =
    { red =
        (let red = c.red in
         if red <= 0. then 0. else if red > 1.0 then 1.0 else red)
    ; green =
        (let green = c.green in
         if green <= 0. then 0. else if green > 1.0 then 1.0 else green)
    ; blue =
        (let blue = c.blue in
         if blue <= 0. then 0. else if blue > 1.0 then 1.0 else blue)
    }

  let add c1 c2 =
    { red = c1.red +. c2.red; green = c1.green +. c2.green; blue = c1.blue +. c2.blue }

  let add_scalar c1 s =
    limit { red = c1.red +. s; green = c1.green +. s; blue = c1.blue +. s }

  let subtract c1 c2 =
    { red = c1.red -. c2.red; green = c1.green -. c2.green; blue = c1.blue -. c2.blue }

  let multiply c1 c2 =
    { red = c1.red *. c2.red; green = c1.green *. c2.green; blue = c1.blue *. c2.blue }

  let multiply_scalar c1 s =
    { red = c1.red *. s; green = c1.green *. s; blue = c1.blue *. s }

  let divide_factor c1 f =
    { red = c1.red /. f; green = c1.green /. f; blue = c1.blue /. f }

  let distance c1 c2 =
    abs_float (c1.red -. c2.red)
    +. abs_float (c1.green -. c2.green)
    +. abs_float (c1.blue -. c2.blue)

  let blend c1 c2 w = add (multiply_scalar c1 (1. -. w)) (multiply_scalar c2 w)

  let brightness c =
    let r = truncate (c.red *. 255.) in
    let g = truncate (c.green *. 255.) in
    let b = truncate (c.blue *. 255.) in
    ((r * 77) + (g * 150) + (b * 29)) lsr 8
end

module Vector = struct
  type t =
    { x : float
    ; mutable y : float
    ; z : float
    }

  let make x y z = { x; y; z }

  (*
  let print ch v = Format.fprintf ch "%f %f %f" v.x v.y v.z
*)

  let magnitude v = sqrt ((v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z))

  let normalize v =
    let m = magnitude v in
    { x = v.x /. m; y = v.y /. m; z = v.z /. m }

  let cross v w =
    { x = (v.y *. w.z) -. (v.z *. w.y)
    ; y = (v.z *. w.x) -. (v.x *. w.z)
    ; z = (v.x *. w.y) -. (v.y *. w.x)
    }

  let dot v w = (v.x *. w.x) +. (v.y *. w.y) +. (v.z *. w.z)

  let add v w = { x = v.x +. w.x; y = v.y +. w.y; z = v.z +. w.z }

  let subtract v w = { x = v.x -. w.x; y = v.y -. w.y; z = v.z -. w.z }

  let multiply_vector v w = { x = v.x *. w.x; y = v.y *. w.y; z = v.z *. w.z }

  let multiply_scalar v w = { x = v.x *. w; y = v.y *. w; z = v.z *. w }
end

module Light = struct
  type t =
    { position : Vector.t
    ; color : Color.t
    ; intensity : float
    }

  let make p c i = { position = p; color = c; intensity = i }
end

module Ray = struct
  type t =
    { position : Vector.t
    ; direction : Vector.t
    }

  let make p d = { position = p; direction = d }
end

module Intersection_info = struct
  type 'a t =
    { shape : 'a
    ; distance : float
    ; position : Vector.t
    ; normal : Vector.t
    ; color : Color.t
    }
end

module Camera = struct
  type t =
    { position : Vector.t
    ; look_at : Vector.t
    ; equator : Vector.t
    ; up : Vector.t
    ; screen : Vector.t
    }

  let make pos look_at up =
    { position = pos
    ; look_at
    ; up
    ; equator = Vector.cross (Vector.normalize look_at) up
    ; screen = Vector.add pos look_at
    }

  let get_ray c vx vy =
    let pos =
      Vector.subtract
        c.screen
        (Vector.subtract
           (Vector.multiply_scalar c.equator vx)
           (Vector.multiply_scalar c.up vy))
    in
    pos.Vector.y <- pos.Vector.y *. -1.;
    let dir = Vector.subtract pos c.position in
    Ray.make pos (Vector.normalize dir)
end

module Background = struct
  type t =
    { color : Color.t
    ; ambience : float
    }

  let make c a = { color = c; ambience = a }
end

module Material = struct
  type t =
    { reflection : float
    ; transparency : float
    ; gloss : float
    ; has_texture : bool
    ; get_color : float -> float -> Color.t
    }

  let wrap_up t =
    let t = mod_float t 2.0 in
    if t < -1. then t +. 2.0 else if t >= 1. then t -. 2.0 else t

  let solid color reflection transparency gloss =
    { reflection
    ; transparency
    ; gloss
    ; has_texture = false
    ; get_color = (fun _ _ -> color)
    }

  let chessboard color_even color_odd reflection transparency gloss density =
    { reflection
    ; transparency
    ; gloss
    ; has_texture = true
    ; get_color =
        (fun u v ->
          let t = wrap_up (u *. density) *. wrap_up (v *. density) in
          if t < 0. then color_even else color_odd)
    }
end

module Shape = struct
  type shape =
    | Sphere of Vector.t * float
    | Plane of Vector.t * float

  type t =
    { shape : shape
    ; material : Material.t
    }

  let make shape material = { shape; material }

  let dummy =
    make
      (Sphere (Vector.make 0. 0. 0., 0.))
      (Material.solid (Color.make 0. 0. 0.) 0. 0. 0.)

  let position s =
    match s.shape with
    | Sphere (p, _) -> p
    | Plane (p, _) -> p

  let intersect s ray =
    match s.shape with
    | Sphere (position, radius) ->
        let dst = Vector.subtract ray.Ray.position position in
        let b = Vector.dot dst ray.Ray.direction in
        let c = Vector.dot dst dst -. (radius *. radius) in
        let d = (b *. b) -. c in
        if d > 0.
        then
          let dist = -.b -. sqrt d in
          let pos =
            Vector.add ray.Ray.position (Vector.multiply_scalar ray.Ray.direction dist)
          in
          Some
            { Intersection_info.shape = s
            ; distance = dist
            ; position = pos
            ; normal = Vector.normalize (Vector.subtract pos position)
            ; color = s.material.Material.get_color 0. 0.
            }
        else None
    | Plane (position, d) ->
        let vd = Vector.dot position ray.Ray.direction in
        if vd = 0.
        then None
        else
          let t = -.(Vector.dot position ray.Ray.position +. d) /. vd in
          if t <= 0.
          then None
          else
            let pos =
              Vector.add ray.Ray.position (Vector.multiply_scalar ray.Ray.direction t)
            in
            Some
              { Intersection_info.shape = s
              ; distance = t
              ; position = pos
              ; normal = position
              ; color =
                  (if s.material.Material.has_texture
                   then
                     let vu =
                       Vector.make
                         position.Vector.y
                         position.Vector.z
                         (-.position.Vector.x)
                     in
                     let vv = Vector.cross vu position in
                     let u = Vector.dot pos vu in
                     let v = Vector.dot pos vv in
                     s.material.Material.get_color u v
                   else s.material.Material.get_color 0. 0.)
              }
end

module Scene = struct
  type t =
    { camera : Camera.t
    ; shapes : Shape.t array
    ; lights : Light.t array
    ; background : Background.t
    }

  let make c s l b = { camera = c; shapes = s; lights = l; background = b }
end

module Engine = struct
  type t =
    { pixel_width : int
    ; pixel_height : int
    ; canvas_width : int
    ; canvas_height : int
    ; render_diffuse : bool
    ; render_shadows : bool
    ; render_highlights : bool
    ; render_reflections : bool
    ; ray_depth : int
    }

  let check_number = ref 0

  let get_reflection_ray p n v =
    let c1 = -.Vector.dot n v in
    let r1 = Vector.add (Vector.multiply_scalar n (2. *. c1)) v in
    Ray.make p r1

  let rec ray_trace options info ray scene depth =
    let old_color =
      Color.multiply_scalar
        info.Intersection_info.color
        scene.Scene.background.Background.ambience
    in
    let color = ref old_color in
    let shininess =
      10. ** (info.Intersection_info.shape.Shape.material.Material.gloss +. 1.)
    in
    let lights = scene.Scene.lights in
    for i = 0 to Array.length lights - 1 do
      let light = lights.(i) in
      let v =
        Vector.normalize
          (Vector.subtract light.Light.position info.Intersection_info.position)
      in
      (if options.render_diffuse
       then
         let l = Vector.dot v info.Intersection_info.normal in
         if l > 0.
         then
           color :=
             Color.add
               !color
               (Color.multiply
                  info.Intersection_info.color
                  (Color.multiply_scalar light.Light.color l)));
      (if depth <= options.ray_depth
       then
         if options.render_reflections
            && info.Intersection_info.shape.Shape.material.Material.reflection > 0.
         then
           let reflection_ray =
             get_reflection_ray
               info.Intersection_info.position
               info.Intersection_info.normal
               ray.Ray.direction
           in
           let col =
             match
               test_intersection reflection_ray scene info.Intersection_info.shape
             with
             | Some ({ Intersection_info.distance = d; _ } as info) when d > 0. ->
                 ray_trace options info reflection_ray scene (depth + 1)
             | _ -> scene.Scene.background.Background.color
           in
           color :=
             Color.blend
               !color
               col
               info.Intersection_info.shape.Shape.material.Material.reflection);
      let shadow_info = ref None in
      if options.render_shadows
      then (
        let shadow_ray = Ray.make info.Intersection_info.position v in
        shadow_info := test_intersection shadow_ray scene info.Intersection_info.shape;
        match !shadow_info with
        | Some info ->
            (*XXX This looks wrong! *)
            let va = Color.multiply_scalar !color 0.5 in
            let db =
              0.5
              *. (info.Intersection_info.shape.Shape.material.Material.transparency ** 0.5)
            in
            color := Color.add_scalar va db
        | None -> ());
      if options.render_highlights
         && !shadow_info <> None
         && info.Intersection_info.shape.Shape.material.Material.gloss > 0.
      then
        (*XXX This looks wrong! *)
        let shape_position = Shape.position info.Intersection_info.shape in
        let lv = Vector.normalize (Vector.subtract shape_position light.Light.position) in
        let e =
          Vector.normalize
            (Vector.subtract scene.Scene.camera.Camera.position shape_position)
        in
        let h = Vector.normalize (Vector.subtract e lv) in
        let gloss_weight =
          max (Vector.dot info.Intersection_info.normal h) 0. ** shininess
        in
        color := Color.add (Color.multiply_scalar light.Light.color gloss_weight) !color
    done;
    Color.limit !color

  and test_intersection ray scene exclude =
    let best = ref None in
    let dist = ref 2000. in
    let shapes = scene.Scene.shapes in
    for i = 0 to Array.length shapes - 1 do
      let shape = shapes.(i) in
      if shape != exclude
      then
        match Shape.intersect shape ray with
        | Some { Intersection_info.distance = d; _ } as v when d >= 0. && d < !dist ->
            best := v;
            dist := d
        | _ -> ()
    done;
    !best

  let get_pixel_color options ray scene =
    match test_intersection ray scene Shape.dummy with
    | Some info -> ray_trace options info ray scene 0
    | None -> scene.Scene.background.Background.color

  let set_pixel _options x y color =
    if x == y then check_number := !check_number + Color.brightness color;
    ( (*
    let pxw = options.pixel_width in
    let pxh = options.pixel_height in
    Format.eprintf "%d %d %d %d %d %a@." (x * pxw) (y * pxh) pxw pxh !check_number Color.print color;
*) )

  let render_scene options scene _canvas =
    check_number := 0;
    (*XXX canvas *)
    let canvas_height = options.canvas_height in
    let canvas_width = options.canvas_width in
    for y = 0 to canvas_height - 1 do
      for x = 0 to canvas_width - 1 do
        let yp = (float y /. float canvas_height *. 2.) -. 1. in
        let xp = (float x /. float canvas_width *. 2.) -. 1. in
        let ray = Camera.get_ray scene.Scene.camera xp yp in
        let color = get_pixel_color options ray scene in
        set_pixel options x y color
      done
    done;
    assert (!check_number = 2321)

  let make
      canvas_width
      canvas_height
      pixel_width
      pixel_height
      render_diffuse
      render_shadows
      render_highlights
      render_reflections
      ray_depth =
    { canvas_width = canvas_width / pixel_width
    ; canvas_height = canvas_height / pixel_height
    ; pixel_width
    ; pixel_height
    ; render_diffuse
    ; render_shadows
    ; render_highlights
    ; render_reflections
    ; ray_depth
    }
end

let render_scene () =
  let camera =
    Camera.make
      (Vector.make 0. 0. (-15.))
      (Vector.make (-0.2) 0. 5.)
      (Vector.make 0. 1. 0.)
  in
  let background = Background.make (Color.make 0.5 0.5 0.5) 0.4 in
  let sphere =
    Shape.make
      (Shape.Sphere (Vector.make (-1.5) 1.5 2., 1.5))
      (Material.solid (Color.make 0. 0.5 0.5) 0.3 0. 2.)
  in
  let sphere1 =
    Shape.make
      (Shape.Sphere (Vector.make 1. 0.25 1., 0.5))
      (Material.solid (Color.make 0.9 0.9 0.9) 0.1 0. 1.5)
  in
  let plane =
    Shape.make
      (Shape.Plane (Vector.normalize (Vector.make 0.1 0.9 (-0.5)), 1.2))
      (Material.chessboard (Color.make 1. 1. 1.) (Color.make 0. 0. 0.) 0.2 0. 1.0 0.7)
  in
  let light = Light.make (Vector.make 5. 10. (-1.)) (Color.make 0.8 0.8 0.8) 10. in
  let light1 = Light.make (Vector.make (-3.) 5. (-15.)) (Color.make 0.8 0.8 0.8) 100. in
  let scene =
    Scene.make camera [| plane; sphere; sphere1 |] [| light; light1 |] background
  in
  let image_width = 100 in
  let image_height = 100 in
  let pixel_size = 5, 5 in
  let render_diffuse = true in
  let render_shadows = true in
  let render_highlights = true in
  let render_reflections = true in
  let ray_depth = 2 in
  let engine =
    Engine.make
      image_width
      image_height
      (fst pixel_size)
      (snd pixel_size)
      render_diffuse
      render_shadows
      render_highlights
      render_reflections
      ray_depth
  in
  Engine.render_scene engine scene None

let _ =
  for _ = 0 to 99 do
    render_scene ()
  done
