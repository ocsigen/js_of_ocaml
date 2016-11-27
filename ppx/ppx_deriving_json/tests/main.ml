module Type_conv = Ppx_type_conv.Type_conv

let l = Ppx_deriving.derivers ()
let _ = ListLabels.iter l ~f:(fun (deriver : Ppx_deriving.deriver) ->
  Type_conv.add deriver.Ppx_deriving.name
    ~str_type_decl:(Type_conv.Generator.make_noarg
                      (fun ~loc:_ ~path (_,i) ->
                         let path = [ path ] in
                         deriver.Ppx_deriving.type_decl_str ~options:[] ~path i))
    ~sig_type_decl:(Type_conv.Generator.make_noarg
                      (fun ~loc:_ ~path (_,i) ->
                         let path = [ path ] in
                         deriver.Ppx_deriving.type_decl_sig ~options:[] ~path i))
  |> Type_conv.ignore)

let () = Toplevel_expect_test.Main.main ()
